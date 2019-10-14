export EDITOR="nano"
export DISABLE_BUNDLER_SETUP=""

export ANDROID_HOME=/home/shibafu/Android/Sdk
export ANDROID_NDK_HOME=/home/shibafu/Android/android-ndk-r14b

export PATH="$HOME/bin:$PATH"
export PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
export PATH="$HOME/.phpenv/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="/opt/android-sdk/platform-tools:/opt/android-sdk/tools:$PATH"
export PATH="$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools:$PATH"

# rbenv
eval "$(rbenv init -)"

# nodenv
eval "$(nodenv init -)"

# phpenv
eval "$(phpenv init -)"
