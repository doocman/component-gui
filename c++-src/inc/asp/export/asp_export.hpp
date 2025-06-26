//
// Created by rvons on 2025-06-26.
//

#ifndef ASPECT_GUI_ASP_EXPORT_ASP_EXPORT_HPP
#define ASPECT_GUI_ASP_EXPORT_ASP_EXPORT_HPP

#if ASP_CXX_MODULE
#define ASP_EXPORT export
#define ASP_EXPORT_BEGIN export {
#define ASP_EXPORT_END }
#else
#define ASP_EXPORT
#define ASP_EXPORT_BEGIN
#define ASP_EXPORT_END
#endif

#endif // ASPECT_GUI_ASP_EXPORT_HPP
