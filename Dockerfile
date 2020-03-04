FROM archlinux:20200205
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm git base-devel
RUN pacman -S --noconfirm go

RUN useradd --create-home packager
# use all possible cores for subsequent package builds
RUN sed -i 's,#MAKEFLAGS="-j2",MAKEFLAGS="-j$(nproc)",g' /etc/makepkg.conf
# don't compress the packages built here
RUN sed -i "s,PKGEXT='.pkg.tar.xz',PKGEXT='.pkg.tar',g" /etc/makepkg.conf
RUN echo '' >> /etc/sudoers
RUN echo 'packager      ALL = NOPASSWD: /usr/sbin/makepkg *' >> /etc/sudoers
RUN echo 'packager      ALL = NOPASSWD: /usr/sbin/pacman *'  >> /etc/sudoers

USER packager
WORKDIR /home/packager/
RUN git clone https://aur.archlinux.org/yay.git
WORKDIR /home/packager/yay/
RUN makepkg -i --noconfirm
RUN yay -S --noconfirm stack-bin

USER root
RUN stack setup --system-ghc

USER packager
RUN yay -S --noconfirm agetpkg-git

USER root
RUN agetpkg -i --debug '^ghc$' 8.6.5 1

#RUN stack install --system-ghc cabal-install
