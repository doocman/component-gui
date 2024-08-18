
#include <cstdlib>
#include <iostream>
#include <thread>

#include <SDL_main.h>

#include <cgui/cgui.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>

int main(int, char **) {
  try {
    auto sdl_context = build(cgui::sdl_context()).value();
    auto video_subsystem = video(sdl_context).value();
    auto main_window =
        build(window(video_subsystem, "Window Title - Hello World", 1024,
                     768)(cgui::sdl_window_resizable))
            .value();

    auto full_area = main_window.area();

    auto text_library = cgui::ft_font_library::init().value();
    auto dpi_info = main_window.dpi().value();
    auto text_font =
        cgui::ft_font_face::init(text_library, "C:\\Windows\\Fonts\\arial.ttf",
                                 static_cast<FT_UInt>(dpi_info.vert),
                                 static_cast<FT_UInt>(dpi_info.hori))
            .value();
    auto text_widget = cgui::text_box_widget(std::move(text_font))
                           .area(full_area)
                           .display("Hello World!");

    bool do_exit{};
    auto renderer = main_window.canvas().value();
    auto gui = cgui::gui_context(renderer).with(text_widget);
    gui.render_direct(renderer);
    renderer.present();
    while (!do_exit) {
      while (cgui::poll_event(sdl_context, [&]<typename T>(T) {
               if constexpr (std::is_same_v<T, cgui::sdl_quit_event>) {
                 do_exit = true;
               }
             }) != 0) {
      }
      std::this_thread::sleep_for(std::chrono::milliseconds(16));
    }
    return EXIT_SUCCESS;
  } catch (std::exception const &e) {
    std::cout << "Uncaught exception: " << e.what() << '\n';
  }
  return EXIT_FAILURE;
}
