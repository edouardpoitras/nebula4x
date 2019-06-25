# Nebula 4x

A 4X game build in Haskell. Insprired by Aurora4x from Steve Walmsley.

DISCLAIMER: This game is not really playable yet and may never become playable.
This was a project I started building with the primary goal of experiencing Haskell development with something other than a small/toy codebase.
I learned a lot along the way and I would make many changes to the way this game was put together (use Type Classes, get rid of web API, native UI, list goes on...).
It hasn't really received a proper GitHub publishing pass so no documentation and some code is pretty ugly.

## Playing

AI has not been implemented so "playing" the game feels more like a simulation where alien life is never discovered.
You can roam your generated galaxy, mine minerals, research upgrades, build bigger/faster/stronger ships, etc.

Run the following command to build:

    stack build

Start the game server with the following command:

    stack exec nebula4x-server

Now that the server is running, you can fire up the [Web UI](https://github.com/edouardpoitras/nebula4x-webui) to play the game.

## Development

Don't forget to format your changes:

    stack exec hfmt -- -w

## Profiling

Some flags to mess around with:

    -XStrict
    -fforce-recomp
    -fexcess-precision
    -optc-ffast-math
    -funfolding-use-threshold=64
    -fexpose-all-unfoldings
    -funbox-strict-fields
    -fspec-constr

To build and run tests with profiling enabled:

    stack test --profile

This will create a .prof file.
You can view .prof files with `profiteur`:

    stack install profiteur
    profiteur nebula4x-test.prof nebula4x-test.html

To also generate a heap profile:

    stack exec -- <path/to/nebula4x-test> +RTS -h -p

This will create a .hp file.
You can convert it into a PostScript file `hp2ps`:

    hp2ps nebula4x-test.hp

You can then view the nebula4x-test.ps file in your favorite PostScript viewer.

## Before Release

- Implement teams/enemies
- AI

## TODO (including UI work)

- Restructure code to make use of Type Classes and cut down on clunky code
- God mode (unlock all research)
- Need to fix all random number generation (especially for large numbers) - need to use proper normal distribution
- Allow for pulling individual bodies instead of entire system every time (mostly work on UI, but API needs to send the body back on certain endpoints - like fuel refining or mass driver for instance)
- Pop-up panels copied down to grid item
- Search body/resource capability (via pop up)
- Clickable bodies on star map with various info on body
- Whenever a planet is clicked in menu display, pop open new star map (as modal?)
- Implement population system
- Implement alien system
- Implement political system
- Implement terraforming system
- Performance: Lots to consider still - ST monad? https://en.wikibooks.org/wiki/Haskell/Mutable_objects
