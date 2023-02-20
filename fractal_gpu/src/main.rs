extern crate clap;
extern crate sdl2;

mod fps;
mod render;
mod debug;
mod render_context;

use render::*;
use debug::DebugWindow;
use render_context::RenderContext;

use clap::{AppSettings, Clap};

use sdl2::event::Event;
use sdl2::event::WindowEvent;
use sdl2::keyboard::Keycode;
use sdl2::mouse::{ MouseButton };

use std::time::{SystemTime};

struct EventLoopContext {
    program_epoch: SystemTime,
    depression: Option<MouseButton>,
}

impl EventLoopContext {
    fn new() -> EventLoopContext {
        EventLoopContext {
            program_epoch: SystemTime::now(),
            depression: None, // :)
        }
    }
}

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
pub struct Opts {
    #[clap(short = 'f', long, default_value = "mandelbrot")]
    shader_name: String,
}

pub fn main() -> Result<(), String> {
    let elc = EventLoopContext::new();
    run(elc);
    Ok(())
}

fn run(mut elc: EventLoopContext) {
    let opts: Opts = Opts::parse();
    let mut context = RenderContext::new();

    let ttf_context = sdl2::ttf::init().map_err(|e| e.to_string()).unwrap();
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("MagicPixel", context.win_width, context.win_height)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    let main_window_id = window.id();

    let (debug_x, debug_y) = window.position();
    let mut debug_window = DebugWindow::new(debug_x, debug_y, &video_subsystem, &ttf_context);

    let mut renderer = GlslRenderer::new(
        "assets/identity.vert".to_string(),
        format!("assets/{}.frag", opts.shader_name),
        window,
        &video_subsystem
    );

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut pan_x = -0.75;
    let mut pan_y = -0.5;
    let mut zoom = 3.0;

    let mut prev_mouse_x = 0;
    let mut prev_mouse_y = 0;

    'running: loop {
        let events: Vec<Event> = event_pump.poll_iter().collect();

        let pan_amount = zoom / 500.0;

        for event in events {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                    pan_y -= pan_amount;
                },
                Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                    pan_x -= pan_amount;
                },
                Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                    pan_y += pan_amount;
                },
                Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                    pan_x += pan_amount;
                },
                Event::MouseMotion { x, y , window_id, .. } => {
                    if window_id == main_window_id {
                        context.mouse_x = x;
                        context.mouse_y = y;
                    }
                },
                Event::MouseButtonDown { x, y , window_id, mouse_btn, .. } => {
                    context.mouse_x = x;
                    context.mouse_y = y;

                    if window_id == main_window_id {
                        elc.depression = Some(mouse_btn);
                    }
                },
                Event::MouseButtonUp { window_id, .. } => {
                    if window_id == main_window_id {
                        elc.depression = None;
                    }
                },
                Event::MouseWheel { y, .. } => {
                    let factor = 0.1;
                    // TODO: pan
                    zoom -= zoom * factor * y as f32;
                },
                Event::Window { win_event: WindowEvent::Leave, .. } => {
                    elc.depression = None;
                },
                Event::Window { win_event: WindowEvent::Enter, .. } => {
                    if event_pump.mouse_state().left() {
                        elc.depression = Some(MouseButton::Left);
                    } else if event_pump.mouse_state().right() {
                        elc.depression = Some(MouseButton::Right);
                    }
                },
                _ => {}
            }
        }

        if elc.depression == Some(MouseButton::Left) {
            let dx = context.mouse_x - prev_mouse_x;
            let dy = context.mouse_y - prev_mouse_y;

            pan_x -= zoom * dx as f32 / context.win_width as f32;
            pan_y -= zoom * dy as f32 / context.win_width as f32;
        }

        let curr_time = get_current_time(&elc);
        debug_window.render(curr_time);

        renderer.render(&context, pan_x, pan_y, zoom);

        prev_mouse_x = context.mouse_x;
        prev_mouse_y = context.mouse_y;
    }
}

fn get_current_time(elc: &EventLoopContext) -> f32 {
    SystemTime::now()
        .duration_since(elc.program_epoch)
        .unwrap()
        .as_secs_f32()
}
