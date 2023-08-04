(define-minor-mode fill-column-color-mode
  "Toggle fill-column-color-mode

When fill-column-color-mode is on,
color-rgb-to-hex of `display-fill-column-indicator-character'
changes according to fill-column, overriding `pspmacs/pspline-cursor-position-face'"
  :lighter nil
  (if fill-column-color-mode
      (unless pspmacs/recolor-fill-column-timer
        (setq pspmacs/display-column-orig-bg-color
              (face-attribute 'fill-column-indicator :background))
        (setq pspmacs/recolor-fill-column-timer
              (run-with-idle-timer 2 t #'pspmacs/recolor-fill-column)))
    (when pspmacs/recolor-fill-column-timer
      (cancel-timer pspmacs/recolor-fill-column-timer)
      (setq pspmacs/recolor-fill-column-timer nil)
      (set-face-background
       'fill-column-indicator
       pspmacs/display-column-orig-bg-color))))
