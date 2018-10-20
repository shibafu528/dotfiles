@echo off

REM aliases / common command
doskey cd=cd /d $*
doskey ls=dir $*
doskey cp=copy $*
doskey mv=move $*
doskey rm=del /p $*

REM aliases / git command
doskey ga=git add $*
doskey gb=git branch $*
doskey gc=git commit $*
doskey gch=git checkout $*
doskey gd=git diff $*
doskey gl=git log --decorate=full --all $*
doskey gs=git status $*
