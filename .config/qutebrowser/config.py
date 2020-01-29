# import dracula.draw

# # Use the Dracula theme: https://github.com/evannagle/qutebrowser-dracula-theme/
# dracula.draw.blood(c, {
#     'spacing': {
#         'vertical': 6,
#         'horizontal': 8
#     },
#     'font': {
#         'family': 'Menlo, Terminus, Monaco, Monospace',
#         'size': 10
#     }
# })

c.auto_save.session = True
c.scrolling.smooth = True
c.session.lazy_restore = True
c.content.autoplay = False

# Scale pages and UI better for hidpi
c.zoom.default = "225%"
c.fonts.hints = "bold 12pt monospace"

# Better default fonts
c.fonts.web.family.standard = "Bitstream Vera Sans"
c.fonts.web.family.serif = "Bitstream Vera Serif"
c.fonts.web.family.sans_serif = "Bitstream Vera Sans"
c.fonts.web.family.fixed = "Fira Mono"
c.fonts.monospace = "Fira Mono"

# Tabs should be separate windows
#c.tabs.tabs_are_windows = True

# Automatically turn on insert mode when a loaded page focuses a text field
c.input.insert_mode.auto_load = True

# Make Ctrl+g quit everything like in Emacs
config.bind('<Ctrl-g>', 'leave-mode', mode='insert')
config.bind('<Ctrl-g>', 'leave-mode', mode='command')
config.bind('<Ctrl-g>', 'leave-mode', mode='prompt')
config.bind('<Ctrl-g>', 'leave-mode', mode='hint')
config.bind('v', 'spawn ~/.dotfiles/bin/umpv {url}')
config.bind('V', 'hint links spawn ~/.dotfiles/bin/umpv {hint-url}')

# Vim-style movement keys in command mode
config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')

# More binding hints here: https://gitlab.com/Kaligule/qutebrowser-emacs-config/blob/master/config.py
