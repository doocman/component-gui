
#include <format>
#include <iostream>
#include <stdexcept>

#include <cgui/cgui.hpp>
#include <cgui/dynamic.hpp>
#include <cgui/embedded/cgui_example_font.hpp>
#include <cgui/ft_fonts.hpp>
#include <cgui/sdl.hpp>

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

    // TODO: Remove these two variables by adding sensible logic into the gui
    int win_width{};
    bool rerender_all{};

    auto gui =
        cgui::gui_context_builder()
            .on_resize([&win_width](auto &&sz, auto &&ws) {
              win_width = cgui::call::width(sz);
              auto &[w, end_button] = ws;
              auto full_area = cgui::box_from_xywh<area_t>(
                  0, 0, cgui::call::width(sz), cgui::call::height(sz));
              // TODO: 240 should be lesser, but we need to define it in
              // "points" rather than pixels.
              auto button_area = cgui::trim_from_below(
                  &full_area, std::min(240, cgui::call::height(full_area)));
              cgui::call::area(end_button, button_area);
              cgui::call::area(w, full_area);
            })
            .widgets(
                cgui::widget_builder()
                    .area(area_t{})
                    .event(
                        cgui::radio_button_trigger()
                            .elements(
                                cgui::dynamic::uni_sized_widget_list_builder()
                                    .displays(cgui::display_per_state(
                                                  cgui::fill_rect()),
                                              cgui::text_renderer(
                                                  std::ref(cached_font))))
                            .build())
                    .build(),
                cgui::widget_builder()
                    .area(area_t{})
                    .event(cgui::momentary_button()
                               .click([&do_exit] { do_exit = true; })
                               .build())
                    .display(cgui::text_renderer(std::ref(cached_font)))
                    .build())
            .build(full_area);
    auto &[list, end_button] = gui.widgets();
    list.event_component().mutate_elements([argc, argv, &full_area, &end_button,
                                            &win_width,
                                            &rerender_all](auto &elements) {
      auto prototype = elements.display_prototype();
      auto &[background, textr] = prototype;
      using enum cgui::radio_button::element_state;
      get<relaxed_off>(background).colour() = {100, 40, 40, 255};
      get<relaxed_on>(background).colour() = {40, 40, 100, 255};
      get<hover_off>(background).colour() = {140, 100, 100, 255};
      get<hover_on>(background).colour() = {100, 100, 140, 255};
      get<hold_off>(background).colour() = {80, 10, 10, 255};
      get<hold_on>(background).colour() = {10, 10, 80, 255};
      textr.text_colour({255, 255, 255, 255});
      decltype(auto) f_prototype = elements.function_prototype();
      auto display_creator = [&prototype, &full_area, &f_prototype, &end_button,
                              &win_width, &rerender_all,
                              &dest = elements.list()](std::string_view text) {
        auto disp = prototype;
        std::get<1>(disp).set_displayed(full_area, text);
        auto f = f_prototype;
        f.get(cgui::radio_button::trigger_on{}) =
            [&end_button, &win_width, &rerender_all,
             ftext = std::format("End program (last button clicked was {})",
                                 text)] {
              auto &[textrenderer] = end_button.displays();
              textrenderer.set_displayed(win_width, 0, ftext);
              rerender_all = true;
            };
        dest.emplace_back(std::move(disp), std::move(f));
      };
      display_creator("This is a list");
      display_creator("containing the");
      display_creator("arguments from");
      display_creator("main:");
      for (int i = 0; i < argc; ++i) {
        display_creator(argv[i]);
      }
    });
    {
      auto &[etxt] = end_button.displays();
      etxt.text_colour({255, 255, 255, 255});
      etxt.set_displayed(win_width, {}, "End program");
    }

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
      // TODO: This first branch should be removed.
      if (rerender_all) {
        gui.render(renderer);
        renderer.present();
      } else if (!cgui::empty_box(to_rerender)) {
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
