export EDITOR="vim"
export DISABLE_BUNDLER_SETUP=""

export ANDROID_HOME=/home/shibafu/Android/Sdk
export ANDROID_NDK_HOME=/home/shibafu/Android/android-ndk-r14b

export PERL_CPANM_OPT="--local-lib=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"

export PATH="$HOME/bin:$PATH"
export PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
export PATH="$HOME/.nodenv/bin:$PATH"
export PATH="$HOME/.phpenv/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="/opt/android-sdk/platform-tools:/opt/android-sdk/tools:$PATH"
export PATH="$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/perl5/bin:$PATH"
export PATH="$HOME/.symfony/bin:$PATH"

# rbenv
eval "$(rbenv init -)"

# nodenv
eval "$(nodenv init -)"

# phpenv
eval "$(phpenv init -)"
