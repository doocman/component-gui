
#include <cstdlib>
#include <iostream>
#include <thread>

#include <SDL_main.h>

#include <cgui/cgui.hpp>
#include <cgui/sdl.hpp>

int main(int, char **) {
  try {
    auto sdl_context = build(cgui::sdl_context()).unwrap();
    auto video_subsystem = video(sdl_context).unwrap();
    auto main_window =
        build(window(video_subsystem, "Window Title - Hello World", 1024, 768)
                  (cgui::sdl_window_resizable))
            .unwrap();

    auto gui_ctx = cgui::gui_context(main_window);

    auto full_area = gui_ctx.area();

    auto text_widget =
        cgui::text_box_widget().area(full_area).display("Hello World!");

    bool do_exit{};
    auto gui = gui_ctx.with(text_widget);
    gui.render();
    while(!do_exit) {
      while(cgui::poll_event(sdl_context, [&] <typename T> (T) {
        if constexpr(std::is_same_v<T, cgui::sdl_quit_event>) {
          do_exit = true;
        }
      }) != 0) {}
      std::this_thread::sleep_for(std::chrono::milliseconds(16));
    }
    return EXIT_SUCCESS;
  } catch (std::exception const &e) {
    std::cout << "Uncought exception: " << e.what() << '\n';
  }
  return EXIT_FAILURE;
}
