#defaults write com.apple.finder DisableAllAnimations -bool true
#defaults write com.apple.dock launchanim -bool false
#defaults write -g NSWindowResizeTime -float 0.001
#defaults write com.apple.dock expose-animation-duration -float 0.1
#defaults write com.apple.dock autohide-time-modifier -float 0
#defaults write com.apple.mail DisableReplyAnimations -bool true

# https://gist.github.com/hofmannsven/ff21749b0e6afc50da458bebbd9989c5
defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
defaults write -g KeyRepeat -int 1         # normal minimum is 2 (30 ms)
