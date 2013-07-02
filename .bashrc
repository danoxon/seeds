# .bashrc - dbarrett

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# aliases for emacs and GNU global
if [ -f ~/.bashemacs ]; then
        . ~/.bashemacs
fi

# User specific aliases and functions
# ---===<<< General Shell Setup >>>===---

umask 022
export CDPATH=.:/home/workspaces/$USER:/home/$USER:/user/dbarrett
NZPATH=/nz:/nz/kit/bin:/nz/kit/sbin:/nz/kit/bin/adm:/nz/src/tools:/nz/kit/sbin/gcc/bin

#/usr/kerberos/bin:/usr/local/bin:/usr/bin:/bin:/usr/X11R6/bin:$HOME/bin:$HOME/bin/borrowed

ECLIPSE_PATH="/"
export JAVA_HOME="/usr"


export PATH=.:${NZPATH}:${ECLIPSE_PATH}:${HOME}/bin:${HOME}/bin/borrowed:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/bin/X11:/opt/net/tools/bin:/opt/accurev/bin

export EDITOR='emacs'

# Pagers settings-
export PAGER="less -ir"
export LESS='-iP ?f%f?m (file %i of %m).:(stdin).\: ?ltline %lt?L/%L.:byte %bB?s/%s..?e (END):?pB (%pB\%)..?x -- Next\: %x. $'
# Allow less to decipher non-text files
{ [ -x /usr/bin/lesspipe.sh ] && export LESSOPEN="|/usr/bin/lesspipe.sh %s"; }\
    || { [ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"; }


export GREP_COLORS='mt=01;34:fn=01;34:ln=04;31:bn=30:se=33'
alias grep='grep --color=auto'


# Special shell behavior
IGNOREEOF=5
unset MAILCHECK
unset TMOUT

# ----  Swap CapsLock for Ctrl, and any other remappings
if [ -f ~/.xmodmap ]; then
    xmodmap ~/.xmodmap
#else
   #echo "No .xmodmap found"
fi

# --- Configure specific packages

# Diab C compiler
[ -f /opt/net/tools/etc/sh_diab ] && . /opt/net/tools/etc/sh_diab


# RTC Jazz
# lscm path needed by Paul Smith's /opt/net/tools/bin/scm'
export SCMPATH="/jazz/scmtools/eclipse"
alias pscm=/opt/net/tools/bin/scm

#if [ -f   . ~/rtc_tools/rtc_helpers ]; then
#  . ~/rtc_tools/rtc_helpers
#fi


# AccuRev
#AC_ENABLE_COMPLETION=yes
#AC_ENABLE_WSPROMPT=yes
export ACCUREV_DIFF_FLAGS="-up"

[ -f /opt/net/tools/etc/sh_accurev ] && . /opt/net/tools/etc/sh_accurev

[ -f ~/.bash_accurev_completion ] && source ~/.bash_accurev_completion 

# ---  IBM specific

# Turn off symantec virus scanner for better compile / indexing times. 
# IBM's  WST will complain eventually.
virusoff()
{
  sudo /etc/init.d/rtvscand stop
  sudo /etc/init.d/symcfgd stop
}

viruson()
{
  sudo /etc/init.d/rtvscand start
  sudo /etc/init.d/symcfgd start
}

### Misc ####
alias reload='. ~/.bashrc'
export GTAGSLIBPATH=.:..:../..:../../..

clean()
{
    SEARCH='.'
    if [ ${1} ]
    then
	SEARCH=${1}
    fi
    find ${SEARCH} \( -name "*~" -or -name ".*~" -or -name "*\#" -or -name "*.core" \) -exec rm -fv {} \;
}


########  VSPU  & VMSPU setup ##########################
# ---===<<< Package-specific setup >>>===---
# Set up for NPS Host
export NUM_SPUS=1
export NZ_HOST="172.29.82.44"  # Using 'localhost' cases "unknown hostname" in nzsql
#export NZ_HOST=`uname -n`
export NZ_MEMORY_MAX_MBYTES=600;
export NZ_MAX_SESSIONS=1000;
export NZ_PASSWORD=password
export NZ_USER=admin
export NZ_AUTOCREATE_DB=1
export NZ_DATABASE=dev   
#export NZ_DATABASE='system';

# export NZROOT=/home/workspaces/<user>/<workspace>/main   #nztree() switches /nz link to kit dir.
# export NZ_KIT_DIR=   #Not needed if links setup for /nz/kit  to workspace's turbo or debug
_kit="${NZ_KIT_DIR:-${NZ_DIR:-/nz}/kit}"
EXTRA_SPACE="/mount/drv2"
export NZ_DATA_DIR=$EXTRA_SPACE/nz/simdata
export NZ_TMP_DIR=$EXTRA_SPACE/tmp
export NZ_LOG_DIR=$EXTRA_SPACE/nz/log


# export NZSTART_ARGS=-i


# VSPU setup how-to
# If re-installing VSPU
# 	Clear /nz/simdata
# 	cat "-priDataPart spu.spa.01.slot.01.dp.0 -priPhysPart disk.spa.01.encl.01.slot.01.pp.1 -mirPhysPart disk.spa.01.encl.01.slot.02.pp.3" >/nz/kit/sys/initTopology
# 	nzinitsystem -B -reinit -uppercase -D /nz/simdata

# amake sim.dev.db  is nzstop, nzinitsystem, nstart.   nzinitsystem clears data.....
#  Use aliases to get non-destructive restarts.
cwdswitch() { cwdkit=`pwd |
              perl -ane '/home\/workspaces\/$ENV{USER}\/([^\/]+)\//; print $1'`
              if [ -z "$cwdkit" ]; then
                echo KIT NOT FOUND\!
              else
                nztree $cwdkit
              fi
            }
 
alias vinit='nzinitsystem -B -reinit -uppercase -D $NZ_DATA_DIR'
alias vstart='nzstart NUM_SPUS=1 -i -sim 1 -D $NZ_DATA_DIR'

alias nzdebug='nzswitch debug'
alias nzturbo='nzswitch turbo'
 
alias new='cwdswitch; nzinitsystem -uppercase -D $NZ_DATA_DIR'
#alias up='cwdswitch; nzstart -i -sim 1 -D $NZ_DATA_DIR'
alias up='nzgo; nzstart -i -sim 1 -D $NZ_DATA_DIR'
alias newup='cwdswitch; nzstart -newSystem -i -sim 1 -D $NZ_DATA_DIR'
alias down=nzstop
alias gdbpm='gdb `which postmaster`'
 

# nzstart NUM_SPUS=1 -i -newSystem -sim 1 -D /nz/simdata 	
export LM_LICENSE_FILE=7789@milkyway
export NZ_MEMORY
#Needed?  vspu hack?
unset LD_LIBRARY_PATH

vspusetup () {
    nzsystem pause -force
    # WLM setup
    nzsystem set -arg host.gkHighPriQueries=36 -force
    nzsystem set -arg host.gkLowPriQueries=36 -force
    nzsystem set -arg host.gkMaxConcurrent=48 -force
    nzsystem set -arg host.gkMaxPerQueue=48 -force
    nzsystem set -arg host.gkQueueThreshold=-1 -force
    nzsystem set -arg host.schedAllowGKandGRA=yes -force
    #  nzsystem set -arg host.schedGRAHorizon=1800 -force
    nzsystem set -arg host.gkMaxConcurrent=100 -force
    #  nzsystem set -arg host.snSchedJobMax=40 -force
    #  nzsystem set -arg system.bladeMemoryMB=8192 -force
    #  nzsystem set -arg system.spuPlanWorkBlocks=2000 -force


    #  nzsystem set -arg system.maxJumboFrameSize=9000 -force
    #  nzsystem set -arg system.maxSpuDistPktsInFlight=100 -force
    #  nzsystem set -arg system.rsSpuWriteFlushMaxBlocks=160 -force
    #  nzsystem set -arg system.spuMaxJobTasks=1024 -force

    nzsystem resume
}



# -----
# Stop here for non-interactive shells
case $- in *i*) : ;; *) return ;; esac

# ---===<<< Interactive Shell Setup >>>===---

# Set the prompt
#export PS1='\W\$ '
# Setting PS1 may prevent  AC_ENABLE_WSPROMPT from puttings workspace info in prompt

# Set PS1 for prompt, and PROMPT_COMMAND for terminal title.
#export PS1='${USER}@${HOSTNAME}:\W\$>'
export PS1='\w\$>'

case $TERM in
  xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0; ${PWD/$HOME/~}\007"'
   # PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
esac

# History setup
HISTSIZE=1000
HISTFILESIZE=5000
HISTCONTROL=ignorespace:erasedups
shopt -s cmdhist histappend

# Other shell options
shopt -s checkhash checkwinsize
shopt -u hostcomplete

#Configure mouse for large desktop
#  Non-linear acceleration and high sensitivity
#xset q | grep -A 1 Pointer
xset m 2 0 
#xset q | grep -A 1 Pointer

# Handy aliases
alias h='history'
alias pd='pushd'
alias la='ls -al'
alias ll='ls -ltr'
alias recent2='find ~/ -type f -mtime -5 |  grep -v "\/\." '
alias recent='find ~/ -type f  -mtime -5 -printf "%T+ %p\n"  |  grep -v "\/\." | sort -nr | cut -c 1-19,31-'


alias rm='rm -f'
alias more='less'
alias diff='diff -ubB'
alias gzip='gzip --verbose'
alias gunzip='gunzip --verbose'
alias env='env | sort'
alias ghist='history | grep'
#alias gs='pushd /nz/src'

# Handy functions

showfuncs() { declare -F ;}

title() { PROMPT_COMMAND='echo -ne "\033]0;"$1"\007"'; }
sudo()  { command sudo "${@:-/bin/bash}"; }


findf() { find . -type f -print0 | xargs -0 grep "$@" /dev/null; }


findch() { find . \( -name '*.[ch]' -o -name '*.cpp' \) -print0 \
             | xargs -0 grep "$@" /dev/null; }

cpto()  { local to="$1"; shift; tar cf - "$@" | (cd "$to"; tar xfBp -); }

# Make and run a single-file c/c++ program
r () { make $1; ./$1;}



############ Netezza helpers ##########################
alias nzdbg1='nzdbg -dbos -Dm dbos:schedrs:rate_limit -Dfall -Dlog true -Dts true'
alias nzdbgoff='nzdbg -dbos -Doff'
alias wlmreg='nzsystem showregistry | egrep -ie "(\.gk|\.gra|\.sn|\.sched)" '
alias sq='nzsqa schedqueues -sys'

########  Amake & Complier settings ##########################
amke() {
    ./amake "$@" 2>&1 | sed -e 's;'`pwd`/';;g'
}
# ccache 
export CCACHE_DIR=$EXTRA_SPACE/workspaces/$USER/ccache
export AOS_CCACHE=ccache  # 1 time: ccache -M 10G 
# parallel amake; if defined empty, figures out a default.
export AOS_J=4


########  Accurev workspace helpers ##########################
export WORKSPACE_HOME=/home/workspaces/dbarrett

em() {
    emacs -nw `ac stat -m -flr`
}

ef() {
    emacs -nw `ac stat -m -flr` `ac stat -k -flr`
}

#Show available workspaces
nzlist() { echo "Current workspace: ";   ls -l ~/nz;
           echo "Available workspaces:" ;     
           (ls -ltr $WORKSPACE_HOME | grep -v accurev)
	   
      }

nznow() {    ls -ld /home/dbarrett/nz | awk '{print $11}'  ;  }


nzgo() { 
   #cd $WORKSPACE_HOME/$WORKSPACE/main/src 
   cd ~/nz/src   # Indirectly set by nztree. /nz -> /home/$user/nz -> $WORKSPACE_HOME/$WORKSPACE/main/src
   printf "Switched to: "
    
   if [ -f .jazzignore ]; then 
    printf "RTC workspace,  "   
   else
     ac info | grep Basis;
   fi
   nznow
}

achist() { ac history -fx $@ | awk -f ~/bin/achistfx.awk;}
alias acmod='ac stat -Rp .'
alias acshow='ac show -a tree'
acstreams() { ac show -mc -s usr-dbarrett streams |  awk '{printf ( "%-30s %s \n",$1,$2)}'; }

# Switch between different workspaces
nztree() { 
        OLDPS3=$PS3
        PS3="Pick a workspace:"
         LIST=`ls -t $WORKSPACE_HOME | grep -v accurev`;
	 select WORKSPACE in $LIST
	 do
	 	 
	   if [ ! -d "$WORKSPACE_HOME/$WORKSPACE" ]; then
             echo "Invalid workspace: $WORKSPACE_HOME/$WORKSPACE"; return 1
           fi
           (cd "$HOME" && rm -f nz && ln -s "$WORKSPACE_HOME/$WORKSPACE/main" nz );
	   
	   break;
	 done
	 
	 nzgo  # go to the newly softlinked workspace
	 PS3=$OLDPS3
         }
         

# Switch between debug and turbo builds
nzswitch() { if [ ! -d "$HOME/nz/." ]; then
               echo "No $HOME/nz workspace set."; return 1
             fi
             if [ ! -d "$HOME/nz/$1" ]; then
               echo "Invalid build type: $1"; return 1
             fi
             (cd "$HOME/nz" && rm -f kit && ln -s "$1" kit \
                && echo "Build type for NZ workspace set to $1");
           }

alias nzdebug='nzswitch debug'
alias nzturbo='nzswitch turbo'

# Completely clean out a source tree and get ready to build
nzcleanbuild () {
  if [ ! -d "src/amake" ]; then
    echo "Not in the root of a source tree."; return 1
  fi
  rm -rf obj/* debug turbo images gcov data simdata
  rm -f log.[1-9]*
  ( cd src/linux_drivers && rm -rf */*.o */*.ko */*.o */.*.cmd */.tmp_versions */*.mod.c )
  ( cd src/amake && ./amake.bsh LINUX )
}



