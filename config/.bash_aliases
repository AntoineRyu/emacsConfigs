###############
### Aliases ###
###############

# Non mujin stuff
alias ll='ls -lart'
alias kk='kill -9 %'
alias gka='gitk --all'
alias fixmerge="emacs \`git diff --name-only  --relative --diff-filter=U\`"
alias fn='find -name '

gkas(){
    gitk --all --select-commit="$1"
}

# Woven stuff
#alias shell="poetry run bootstrap shell"

vnc(){
    vncviewer controller"$1"
}

#######################
### Auto completers ###
#######################

# our handler that returns choices by populating Bash array COMPREPLY
# (filtered by the currently entered word ($2) via compgen builtin)
_gitbranch_complete() {
    branches=$(git branch -l | cut -c3-)
    COMPREPLY=($(compgen -W "$branches" -- "$2"))
}

# Jhbuild names autocomplete
_jhbuild_complete()
{
	cur=${COMP_WORDS[COMP_CWORD]}
    command_list="`jhbuild list -a`"	
	for i in $command_list; do
		if [ -z "${i/$cur*}" ]; then
			COMPREPLY=( ${COMPREPLY[@]} $i )
		fi
	done
}

# Commonly used checkoutroot directories autocomplete
_checkoutroot_complete()
{
    commonDirList="tsl_robot"
    COMPREPLY=($(compgen -W "$commonDirList" -- "$2"))
}


_remote_bootstrap_shell() 
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts=$(grep '^Host' ~/.ssh/config ~/.ssh/config.d/* 2>/dev/null | grep -v '[?*]' | cut -d ' ' -f 2-)

    COMPREPLY=( $(compgen -W " $opts" -- ${cur}) )
    return 0
}

#########################
### Utility functions ###
#########################

# Quickly cd into checkoutroot common directories
ccd (){
    if [ "$1" == 'jhbuild' ]; then
        path=/home/antoine-rioux/jhdbuild/docker
    else
        path=/home/antoine-rioux/checkoutroot/"$1"/"$1"
    fi
    cd $path
}
complete -F _checkoutroot_complete ccd

# # jhbuild and rsync 
# rsyncrepo(){
#     if [ $# -eq 0 ]; then
#         echo "Usage: rsyncrepo TARGET_ADDRESS JHBUILD_REPO"
#         return
#     elif [ $# -eq 1 ]; then
#         echo "Please provide a jhbuild repository to rsync to or use one of the following quick options:"
#         echo "  1 - mujinrobotbridgescpp"
#         echo "  2 - openrave"
#         echo "  3 - ikfactpp"
#         echo "  4 - mujinplanningcommoncpp"
#         echo "  5 - mujincontrollercommoncpp"

#         read MODULE
#     else
#         MODULE=$2
#     fi

#     if [ "$MODULE" == 1 ]; then
#        MODULE="mujinrobotbridgescpp"
#     elif [ "$MODULE" == 2 ]; then
#         MODULE="openrave"
#     elif [ "$MODULE" == 3 ]; then
#         MODULE="ikfastcpp"
#     elif [ "$MODULE" == 4 ]; then
#         MODULE="mujinplanningcommoncpp"
#     elif [ "$MODULE" == 5 ]; then
#         MODULE="mujincontrollercommoncpp"
#     fi

#     SEND_DEBUG=${3:-yes}
#     if [[ "$SEND_DEBUG" =~ ^(?:yes\b|no\b) ]]; then
#        echo "DEBUG argument not supported: \""$SEND_DEBUG"\" overwritting with \"no\""
#        SEND_DEBUG="no"
#     fi
       
#     if [[ "$1" =~ ^[0-9]+$ ]]; then
#         REMOTE="c+controller"$1
#     elif [[ "$1" =~ ^gw[0-9]+$ ]]; then
#         REMOTE="c+gw+controller"${1:2}
#     elif [[ "$1" =~ ^\.[0-9]+$ ]]; then
#         REMOTE=172.17.0$1
#     else
#         REMOTE=$1
#     fi
#     echo "rsync \""$MODULE"\" to \""$REMOTE"\" DEBUG="$SEND_DEBUG

#     path=$(pwd)
#     cd $MUJINJH_APPCONTROLLER_HOME/docker
#     make BUILD=release PRESET=ecs8xxx JOBS=4 jhbuild-run rsync BUILD_OPTS="--no-network" CMD="jhbuild buildone -n "$MODULE"" REMOTE=$REMOTE MODULES="$MODULE" DEBUG="$SEND_DEBUG"
#     cd $path
# }
# complete -F _jhbuild_complete rsyncrepo

# # Replace a jhbuild repository with a target one. Meant only for testing environment! Completely overwrites history of the replaced repository with the other!
# replacerepo(){
#     if [ $# -eq 0 ]; then
#         echo "Usage: replacerepo REPLACE_REPO WITH_THIS_REPO"
#         return
#     elif [ $# -eq 1 ]; then
#         echo "Please provide a target repository to copy"
#         return
#     fi

#     path=$(pwd)
#     cd $MUJINJH_APPCONTROLLER_HOME
#     git fetch
#     git checkout "$1"
#     git reset --hard origin/"$2"
#     git push origin HEAD -f
#     git submodule update --init --recursive
#     git submodule foreach --recursive "git push origin HEAD:'$1' -f"
#     git submodule foreach --recursive "git checkout '$1'"
#     git submodule foreach --recursive "git reset --hard origin/'$1'"
#     cd $path
#     if [ -f "/tmp/$2" ]; then
#        rm /tmp/$2
#     fi
# }
# complete -F _gitbranch_complete replacerepo

# advancerepo(){

#     path=$(pwd)

#     git submodule foreach --recursive "git checkout '$1'"
#     git submodule foreach --recursive "git reset --hard origin/'$1'"
#     branch_name=$(git symbolic-ref HEAD 2>/dev/null | cut -d"/" -f 3)
#     git branch --set-upstream-to=origin/$branch_name $branch_name
#     if mujin_jhbuildcommon_advancemodule.bash "$1"; then
#         echo "${path##*/}:$branch_name" >> /tmp/$1
#         cd $MUJINJH_APPCONTROLLER_HOME
#         mujin_jhbuildcommon_advancesubmodules.bash "$1"
#     else
#         # Retry
#         git submodule foreach --recursive "git checkout '$1'"
#         git submodule foreach --recursive "git reset --hard origin/'$1'"
#         if mujin_jhbuildcommon_advancemodule.bash "$1"; then
#             echo "${path##*/}:$branch_name" >> /tmp/$1
#             cd $MUJINJH_APPCONTROLLER_HOME
#             mujin_jhbuildcommon_advancesubmodules.bash "$1"
#         fi
#     fi    
#     cd $path
# }
# complete -F _gitbranch_complete advancerepo

# diffbranches() {
#     git checkout "$1"
#     mujin_jhbuildcommon_initjhbuild.bash
#     cp -r modulesets/ /tmp/"$1"
#     git checkout "$2"
#     mujin_jhbuildcommon_initjhbuild.bash
#     diff -r modulesets/ /tmp/"$1"
# }
# complete -F _jhbuild_complete diffbranches

replacesim() {
     path=$(pwd)
     
     cd ~/checkoutroot/
     ./arene-robot-sim/Unity/release.py tsl tsl_simulator
     rm -r tsl_simulator
     unzip tsl_simulator.zip
     rm tsl_simulator.zip
     cd tsl_simulator
     poetry install

     cd $path
}

shell(){
    if [ $# -eq 0 ]; then
        poetry run bootstrap shell
    elif [ $# -eq 1 ]; then
        poetry run bootstrap shell "$1"
    else
        poetry run bootstrap shell "$1" "$2"
    fi
}
complete -F _remote_bootstrap_shell shell
