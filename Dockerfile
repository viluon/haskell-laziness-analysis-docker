FROM archlinux:20200205 AS arch-ghc-build-env

RUN pacman -Sy --noconfirm archlinux-keyring
RUN pacman -Su --noconfirm
RUN pacman -S --noconfirm reflector
RUN sh -c 'reflector --score 20 -f 8 --sort rate > /etc/pacman.d/mirrorlist'
RUN pacman -S --noconfirm git base-devel go python python-sphinx libedit numactl exa

# use all possible cores for subsequent package builds
RUN sed -i 's,#MAKEFLAGS="-j2",MAKEFLAGS="-j$(nproc)",g' /etc/makepkg.conf
# don't compress the packages built here
RUN sed -i "s,PKGEXT='.pkg.tar.xz',PKGEXT='.pkg.tar',g" /etc/makepkg.conf
# set up the packager user
RUN useradd --create-home packager
COPY bashrc.sh /home/packager/.bashrc
COPY packager-actions /etc/sudoers.d/

USER packager
WORKDIR /home/packager/
RUN git clone https://aur.archlinux.org/yay.git
WORKDIR /home/packager/yay/
RUN makepkg -i --noconfirm

WORKDIR /home/packager/
RUN yay -S --noconfirm ghcup-git stack-bin

RUN ghcup install 8.6.5
RUN ghcup set 8.6.5
RUN ghcup install-cabal


USER root
RUN ln -s /home/packager/.ghcup/bin/ghc     /usr/sbin/ghc
RUN ln -s /home/packager/.ghcup/bin/ghc-pkg /usr/sbin/ghc-pkg
RUN stack install --system-ghc alex happy
RUN ln -s /root/.local/bin/alex             /usr/sbin/alex
RUN ln -s /root/.local/bin/happy            /usr/sbin/happy
RUN chmod a+x  /root/
RUN chmod a+x  /root/.local/
RUN chmod a+x  /root/.local/bin/
RUN chmod a+rx /root/.local/bin/alex
RUN chmod a+rx /root/.local/bin/happy


# build GHC
FROM arch-ghc-build-env AS arch-ghc-cleanbuild
USER packager
WORKDIR /home/packager/
