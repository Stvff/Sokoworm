# Sokoworm
![Screenshot of the first level](/worm.png "Screenshot of the first level")\
A very short sokoban, made for the 2024 4MB gamejam.

More info can be found on:\
https://vstvff.itch.io/sokoworm

build with nasm:
```
$ nasm -f bin -o sokoworm worm.nasm
$ chmod +x sokoworm
```

<!--
1: f f f f f f f
2: f f f f f f f
3: f f d e e e d d f f f f r e e e r
4: d f f r f r e d d e e r r e r f f d d d
5: d f f r r f r e e d d d
6: r r f r e r e r f d d e r r e d d f r

echo "f f f f f f f f f f f f f f f f d e e e d d f f f f r e e e r d f f r f r e d d e e r r e r f f d d d d f f r r f r e e d d d r r f r e r e r f d d e r r e d d f r" | ./sokoworm
See if you can improve this :3
-->
