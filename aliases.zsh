rc() {
	~/bin/rcrc $*
	source ~/.rc_sh
}

alias rcmpv='rc -e mpv -q'
alias rcm='rcmpv'
