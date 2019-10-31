# Aliases

# Non mujin stuff
alias ll='ls -lart'
alias remacs='emacs -Q -l ~/emacsConfigs/init.el'
alias kk='kill -9 %'
alias gka='gitk --all'
alias gks='gitk --all --select--commit='
alias gg=git

# Mujin stuff
alias plani="mujin_planningserver_startbackendmaster.py --logdir '' --loglevel 'info'"
alias pland="mujin_planningserver_startbackendmaster.py --logdir '' --loglevel 'debug'"
alias ui="mujin_teachworkerui_start.py --usermode dev"
alias emax='emacs -Q -l ~/dotemacs/init.el'

# Functions
rsyncrepo(){
    if [[ "$1" =~ ^[0-9]+$ ]]; then
        REMOTE="c+controller"$1
        echo "rsync to REMOTE="$REMOTE
    elif [[ "$1" =~ ^\.[0-9]+$ ]]; then
        REMOTE=172.17.0$1
        echo "rsync to REMOTE="$REMOTE
    else
        REMOTE=$1
        echo "rsync to REMOTE="$REMOTE
    fi
    path=$(pwd)
    cd $MUJINJH_APPTEACHWORKER_HOME/docker
    make BUILD=release  PRESET=ecs8xxx JOBS=4 jhbuild-run rsync BUILD_OPTS="--no-network" CMD="jhbuild buildone -n "$2"" REMOTE=$REMOTE MODULES="$2" DEBUG=yes
    cd $path
}

# Don't forget to use origin/BRANCH for proper update.
replacerepo(){
    cd $MUJINJH_APPTEACHWORKER_HOME
    git fetch
    git checkout "$1"
    git reset --hard origin/"$2"
    git push origin HEAD -f
    git submodule foreach --recursive "git push origin HEAD:'$1' -f"
    git submodule foreach --recursive "git checkout '$1'"
    git submodule foreach --recursive "git reset --hard origin/'$1'"
}

advancerepo(){
    git submodule foreach --recursive "git checkout '$1'"
    git submodule foreach --recursive "git reset --hard origin/'$1'"
    branch_name=$(git symbolic-ref HEAD 2>/dev/null | cut -d"/" -f 3)
    git branch --set-upstream-to=origin/$branch_name $branch_name
    mujin_jhbuildcommon_advancemodule.bash "$1" # might fail due to branch not set upstream
    path=$(pwd)
    cd $MUJINJH_APPTEACHWORKER_HOME
    mujin_jhbuildcommon_advancesubmodules.bash "$1"
    cd $path
}

diffbranches() {
    git checkout "$1"
    mujin_jhbuildcommon_initjhbuild.bash
    cp -r modulesets/ /tmp/"$1"
    git checkout "$2"
    mujin_jhbuildcommon_initjhbuild.bash
    diff -r modulesets/ /tmp/"$1"
}
