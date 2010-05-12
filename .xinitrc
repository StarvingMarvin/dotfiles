
#~/bin/wallpapers &
xrandr --output DVI-0 --right-of VGA-0 &
xsetroot -cursor_name left_ptr -solid black &
trayer --edge top --align left --height 8 --widthtype request --expand true --SetPartialStrut true --transparent true --alpha 1 --tint 0x000000 --margin 1152&
#stalonetray --geometry 7x1+1166-1008 -bg "#000000" --grow-gravity W -i 16 --skip-taskbar yes --sticky yes --window-strut top &
start-pulseaudio-x11 &

firefox &
klipper &
opera &
psi &
kmix &

exec xmonad 

