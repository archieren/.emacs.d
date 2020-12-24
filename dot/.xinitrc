export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=zh_CN.UTF-8

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx &

# for urxvt
xrdb ~/.Xresources

# export GDK_BACKEND=x11
export XDG_SESSION_TYPE=x11
exec gnome-session

