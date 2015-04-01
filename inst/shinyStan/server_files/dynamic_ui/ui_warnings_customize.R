# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.



output$ui_warning_thresholds_customize <- renderUI({
  threshold_label <- "Warning threshold"
  tags$div(
    splitLayout(
      helpText(threshold_label), helpText(threshold_label), helpText(threshold_label),
      cellArgs = list(style = "line-height: 0px;")
    ),
    splitLayout(sliderInput("n_eff_threshold", width = "67%", label = "", ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
                sliderInput("mcse_threshold", width = "67%", label = "", ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
                sliderInput("rhat_threshold", width = "67%", label = "", ticks = FALSE, value = 1.1, min = 1, max = 1.2, step = 0.01),
                cellArgs = list(style = "line-height: 0px;")
    )
  )
})
