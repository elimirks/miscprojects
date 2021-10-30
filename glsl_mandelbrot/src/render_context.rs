pub struct RenderContext {
    pub win_width: u32,
    pub win_height: u32,
    pub mouse_x: i32,
    pub mouse_y: i32,
}

impl RenderContext {
    pub fn new() -> RenderContext {
        let win_width = 1000;
        let win_height = 1000;

        RenderContext {
            win_width: win_width,
            win_height: win_height,
            mouse_x: 0,
            mouse_y: 0,
        }
    }
}
