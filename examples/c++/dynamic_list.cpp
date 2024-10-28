
#include <iostream>
#include <stdexcept>

#include <cgui/cgui.hpp>
#include <cgui/embedded/cgui_example_font.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>
#include <cgui/dynamic.hpp>

int main(int argc, char **argv) {
  try {
    auto sdl_context = build(cgui::sdl_context()).value();
    auto video_subsystem = video(sdl_context).value();
    auto main_window =
        build(window(video_subsystem, "Window Title - Hello World", 1024,
                     768)(cgui::sdl_window_resizable))
            .value();

    auto full_area = main_window.local_area();

    auto text_library = cgui::ft_font_library::init().value();
    auto dpi_info =
        main_window.dpi().value_or(cgui::sdl_display_dpi{72.f, 72.f, 72.f});
    auto text_font = cgui::ft_font_face::init(
                         text_library, cgui::embedded::cgui_example_font(),
                         static_cast<FT_UInt>(dpi_info.vert),
                         static_cast<FT_UInt>(dpi_info.hori))
                         .value();
    auto cached_font = cgui::cached_font(std::move(text_font));

    bool do_exit{};
    auto renderer = main_window.canvas().value();

    using area_t = decltype(full_area);

    auto gui =
        cgui::gui_context_builder()
            .on_resize([] (auto&& sz, auto&& ws) {
              auto& [w] = ws;
              cgui::call::area(w, cgui::box_from_xywh<area_t>(0, 0, cgui::call::width(sz), cgui::call::height(sz)));
            })
            .widgets(cgui::widget_builder()
                         .area(area_t{})
                         .event(cgui::radio_button_trigger().elements(
                             cgui::dynamic::uni_sized_widget_list_builder().displays(
                                 cgui::display_per_state(cgui::fill_rect()),
                                 cgui::text_renderer(std::ref(cached_font))
                                 )
                             ).build())
                         .build())
            .build(full_area);
    auto& [list] = gui.widgets();
    list.event_component().mutate_elements([argc, argv, &full_area] (auto& elements) {
      auto prototype = elements.display_prototype();
      auto& [background, textr] = prototype;
      using enum cgui::radio_button::element_state;
      get<relaxed_off>(background).colour() = {100, 40, 40, 255};
      get<relaxed_on>(background).colour() = {40, 40, 100, 255};
      get<hover_off>(background).colour() = {140, 100, 100, 255};
      get<hover_on>(background).colour() = {100, 100, 140, 255};
      get<hold_off>(background).colour() = {80, 10, 10, 255};
      get<hold_on>(background).colour() = {10, 10, 80, 255};
      textr.text_colour({255, 255, 255, 255});
      auto display_creator = [&prototype, &full_area] (std::string_view text) {
        auto res = prototype;
        std::get<1>(res).set_displayed(full_area, text);
        return res;
      };
      elements.list().emplace_back(display_creator("This is a list"));
      elements.list().emplace_back(display_creator("containing the"));
      elements.list().emplace_back(display_creator("arguments from"));
      elements.list().emplace_back(display_creator("main:"));
      for(int i = 0; i < argc; ++i) {
        elements.list().emplace_back(display_creator(argv[i]));
      }
    });

    gui.render(renderer);
    renderer.present();
    using namespace std::chrono;
    constexpr auto run_interval = duration_cast<nanoseconds>(1s) / 60;

    auto next_run = steady_clock::now() + run_interval;
    while (!do_exit) {
      SDL_Rect to_rerender{};
      while (cgui::poll_event(sdl_context, [&]<typename T>(T e) {
               if constexpr (std::is_same_v<T, cgui::sdl_quit_event>) {
                 do_exit = true;
               } else if constexpr (cgui::has_handle<decltype(gui), T>) {
                 auto new_rerender = gui.handle(std::move(e));
                 to_rerender = cgui::box_add(to_rerender, new_rerender);
               }
               cgui::unused(e);
             }) != 0) {
      }
      if (!cgui::empty_box(to_rerender)) {
        gui.render(renderer, to_rerender);
        renderer.present();
      }
      std::this_thread::sleep_until(next_run);
      next_run += run_interval;
    }

    return EXIT_SUCCESS;

  } catch (std::exception const &e) {
    std::cout << "Uncaught exception: " << e.what() << '\n';
  } catch (...) {
    std::cout << "Uncaught exception UNKNOWN\n";
  }
  return EXIT_FAILURE;
}
