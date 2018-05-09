

# 这是我的Emacs的配置文件，來至Steven Purcell的配置。目前正在慢慢的学习、调整。
# 不准备作为一个可公用的东西，所以我将很多环境相关的东西删掉了。我只用archlinux。
# WWW的开发环境，留着无妨，但可能会被弱化。
# 向高手们（Rdeguardtoo 等）致敬!

Emacs itself comes with support for many programming languages. This
config adds improved defaults and extended support for the following, listed
in the approximate order of how much I use them, from most to least:

** Haskell / Elm
** Ruby / Ruby on Rails
** CSS / LESS / SASS / SCSS
** Javascript / Typescript / Coffeescript
** HTML / HAML / Markdown / Textile / ERB
** Rust
** Python
** Clojure (with Cider and nRepl)
** Common Lisp (with Slime)
** PHP
** Erlang

In particular, there's a nice config for *autocompletion* with
[company](https://company-mode.github.io/), and
[flycheck](http://www.flycheck.org) is used to immediately highlight
syntax errors in Ruby, Python, Javascript, Haskell and a number of
other languages.

## Supported Emacs versions

The config should run on Emacs 25 or greater and is designed to
degrade smoothly - see the Travis build - but note that Emacs 24.5 and
above is required for an increasing number of key packages, so to get
full functionality you should use the latest Emacs version available
to you.

## Other requirements

To make the most of the programming language-specific support in this
config, further programs will likely be required, particularly those
that [flycheck](https://github.com/flycheck/flycheck) uses to provide
on-the-fly syntax checking.

The Font Awesome 5 should be installed.
