extern crate gl;

use crate::{RenderContext};

use sdl2::render::Canvas;
use sdl2::video::Window;

use std::ffi::CString;

use gl::types::{GLfloat, GLenum, GLuint, GLint, GLchar, GLsizeiptr};

pub struct GlslRenderer {
    vertex_array_id: GLuint,
    program_id: GLuint,
    canvas: Canvas<Window>,
}

impl GlslRenderer {
    pub fn new(
        vert_shader_path: String,
        frag_shader_path: String,
        window: Window,
        video_subsystem: &sdl2::VideoSubsystem,
    ) -> GlslRenderer {
        // initialization
        gl::load_with(|name| video_subsystem.gl_get_proc_address(name) as *const _);

        let canvas = window.into_canvas()
            .index(find_sdl_gl_driver().expect("Could not find GL driver"))
            .build()
            .unwrap();

        // sdl::render creates a context for you, if you use a Canvas you need to use it.
        let _ = canvas.window().gl_set_context_to_current();

        let vert_shader_src = std::fs::read_to_string(vert_shader_path).expect("shader not found!");
        let vert_shader_id = compile_shader(&vert_shader_src, gl::VERTEX_SHADER);

        let frag_shader_src = std::fs::read_to_string(frag_shader_path).expect("shader not found!");
        let frag_shader_id = compile_shader(&frag_shader_src, gl::FRAGMENT_SHADER);

        let program_id = link_program(vert_shader_id, frag_shader_id);

        GlslRenderer {
            vertex_array_id: GlslRenderer::load_fullscreen_vertex_buffer(),
            program_id: program_id,
            canvas: canvas,
        }
    }

    fn load_fullscreen_vertex_buffer() -> GLuint {
        let mut vertex_array_id: GLuint = 0;

        let vertices: [GLfloat; 18] = [
            // Top/left triangle
            -1.0,  1.0, 0.0,
            -1.0, -1.0, 0.0,
            1.0,  1.0, 0.0,
            // Bottom/right triangle
            1.0,  1.0, 0.0,
            1.0, -1.0, 0.0,
            -1.0, -1.0, 0.0,
        ];

        unsafe {
            gl::GenVertexArrays(1, &mut vertex_array_id);
            gl::BindVertexArray(vertex_array_id);

            // This will identify our vertex buffer
            let mut vertex_buffer_id: GLuint = 0;
            // Generate 1 buffer, put the resulting identifier in vertexbuffer
            gl::GenBuffers(1, &mut vertex_buffer_id);
            // The following commands will talk about our 'vertexbuffer' buffer
            gl::BindBuffer(gl::ARRAY_BUFFER, vertex_buffer_id);
            // Give our vertices to OpenGL.
            gl::BufferData(
                gl::ARRAY_BUFFER,
                (vertices.len() * std::mem::size_of::<GLfloat>()) as GLsizeiptr,
                std::mem::transmute(&vertices[0]),
                gl::STATIC_DRAW
            );
        }

        vertex_array_id
    }

    fn get_uniform_location(&mut self, name: &str) -> GLint {
        unsafe {
            let c_str = CString::new(name.as_bytes()).unwrap();
            gl::GetUniformLocation(self.program_id, c_str.as_ptr())
        }
    }

    fn set_uniform_f32(&mut self, name: &str, value: f32) {
        let loc = self.get_uniform_location(name);

        unsafe {
            gl::Uniform1f(loc, value);
        }
    }

    pub fn set_uniform_i32(&mut self, name: &str, value: i32) {
        let loc = self.get_uniform_location(name);

        unsafe {
            gl::Uniform1i(loc, value);
        }
    }

    pub fn render(&mut self, context: &RenderContext, pan_x: f32, pan_y: f32, zoom: f32) {
        self.canvas.clear();

        self.set_uniform_i32("win_height", context.win_height as i32);
        self.set_uniform_i32("win_width", context.win_width as i32);

        self.set_uniform_f32("pan_x", pan_x);
        self.set_uniform_f32("pan_y", pan_y);

        self.set_uniform_f32("zoom", zoom);

        unsafe {
            gl::UseProgram(self.program_id);

            // 1st attribute buffer : vertices
            gl::EnableVertexAttribArray(0);
            gl::BindBuffer(gl::ARRAY_BUFFER, self.vertex_array_id);
            gl::VertexAttribPointer(
                0,                // attribute 0. No particular reason for 0, but must match the layout in the shader.
                3,                // size
                gl::FLOAT,        // type
                gl::FALSE,        // normalized?
                0,                // stride
                std::ptr::null(), // array buffer offset
            );

            // Starting from vertex 0; 6 vertices total -> 2 triangles
            gl::DrawArrays(gl::TRIANGLES, 0, 6);
            gl::DisableVertexAttribArray(0);
        }

        self.canvas.present();
    }
}

fn link_program(vert_shader_id: GLuint, frag_shader_id: GLuint) -> GLuint {
    let program_id = unsafe { gl::CreateProgram() };

    let successful: bool;

    unsafe {
        gl::AttachShader(program_id, vert_shader_id);
        gl::AttachShader(program_id, frag_shader_id);
        gl::LinkProgram(program_id);

        successful = {
            let mut result: GLint = 0;
            gl::GetProgramiv(program_id, gl::LINK_STATUS, &mut result);
            result != 0
        };
    }

    if successful {
        program_id
    } else {
        panic!("Failed to link the program:\n{}", get_link_log(program_id))
    }
}

fn get_link_log(program_id: GLuint) -> String {
    let mut len = 0;
    unsafe { gl::GetProgramiv(program_id, gl::INFO_LOG_LENGTH, &mut len) };
    assert!(len > 0);

    let mut buf = Vec::with_capacity(len as usize);
    let buf_ptr = buf.as_mut_ptr() as *mut gl::types::GLchar;
    unsafe {
        gl::GetProgramInfoLog(program_id, len, std::ptr::null_mut(), buf_ptr);
        buf.set_len(len as usize);
    };

    match String::from_utf8(buf) {
        Ok(log) => log,
        Err(vec) => panic!("Could not convert link log from buffer: {}", vec)
    }
}

fn compile_shader(src: &str, ty: GLenum) -> GLuint {
    let shader;
    unsafe {
        shader = gl::CreateShader(ty);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes()).unwrap();
        gl::ShaderSource(shader, 1, &c_str.as_ptr(), std::ptr::null());
        gl::CompileShader(shader);

        // Get the compile status
        let mut status = gl::FALSE as GLint;
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != (gl::TRUE as GLint) {
            let mut len = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);

            let mut buf = vec![32u8; len as usize];

            gl::GetShaderInfoLog(
                shader,
                len,
                std::ptr::null_mut(),
                buf.as_mut_ptr() as *mut GLchar,
            );

            let message = String::from_utf8(buf.as_slice().to_vec())
                .ok()
                .expect("ShaderInfoLog not valid utf8");

            println!("Shader compile error:\n{}", message);

            panic!("Rubbish shader, rubbish programmer");
        }
    }
    shader
}

fn find_sdl_gl_driver() -> Option<u32> {
    for (index, item) in sdl2::render::drivers().enumerate() {
        if item.name == "opengl" {
            return Some(index as u32);
        }
    }
    None
}
