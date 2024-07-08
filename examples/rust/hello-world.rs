extern crate sdl2;
extern crate component_gui;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;

fn main() {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem
        .window("Window Title - Hello world", 1024, 768)
        .resizable()
        .build()
        .unwrap();

    let mut gui_ctx = component_gui::Sdl::new(&window);
    let full_area = gui_ctx.screen_area();
    let _text_box = component_gui::text_box_widget(&gui_ctx).area(full_area).display("Hello world!").build().unwrap();

    let mut event_pump = sdl_context.event_pump().unwrap();

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => {
                    break 'running;
                }
                _ => {}
            }
        }

        ::std::thread::sleep(::std::time::Duration::new(0, 1_000_000_000u32 / 60));
    }
}
