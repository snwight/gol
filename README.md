gol
===
Conway's Game of Life as an OTP-compliant application 


Start an Erlang shell from the gol/ root directory:

	erl -pa ./ebin -gol

From the Erlang shell, start the application:

	application:start(gol).
	
The default world grid is 60 by 80 by 1 cell (a 2-D planar world), set in Erlang environment variable 'dimensions': {dimensions, [60, 80, 1]}, where the three element list represents [row count, column count, depth].

One-dimensional (i.e. linear) worlds are now supported, simply set rows to 1: {dimensions, [1, 80, 1]}. 

The default rule set is Conway's 'B3/S23', set in Erlang environment variable 'rules':
{rules, {[3], [2, 3]}}. 

 A collection of predefined rules (see below) is now available via the new rules() function, e.g.:

	gol_server:rule(rule90).

Seed the world either with a custom-defined list of cells to activate:

	gol_server:seed([ CellKey1, CellKey2, CellKey3, ...CellKeyN ]).

where CellKey is an atom in the following pattern:  'Row:Col'

...or use the example seed sets described below, which are available using keywords, e.g.:

	gol_server:seed(fumarole).

Accepted keys are lowercase versions of the named seed patterns below

Placement of the seed pattern defaults to center of the world but that's roughly 
adjustable using an additional parameter to the seed function:

	gol_server:seed(left, fumarole).

Accepted locations are [top, bottom, left, right, ctr] 

	gol_server:display().

Prints current state of the world grid

	gol_server:clear().

Empties and displays the world grid

	gol_server:tick().

Executes a single global clock increment and displays refreshed world grid

	gol_server:run(<<N>>).

...Where N sets a finite number of ticks to execute, defaulting to 1000 ms intervals - 
tune this delay using erlang environment variable tempo, e.g. to drop that to 50 ms:
{tempo, 50} 


===

Example seed states of interest (lifted from wikipedia examples) - 
try these out using their names as keys to gol_server:seed(..name...)
 
Still Lifes:

      block: 
      ['2:2', '2:3', '3:2', '3:3']
      beehive: 
      ['2:3', '2:4', '3:2', '3:5', '4:3', '4:4']
      loaf:
      ['2:3', '2:4', '3:2', '3:5', '4:3', '4:5', '5:4']
      boat:
      ['2:2', '2:3', '3:2', '3:4', '4:3']

Oscillators:

      blinker (period 2):
      ['3:2', '3:3', '3:4']
      toad (period 2):
      ['3:3', '3:4', '3:5', '4:2', '4:3', '4:4']
      beacon (period 2):
      ['2:2', '2:3', '3:2', '4:5', '5:4', '5:5']
      fumarole (period 5):
      ['1:4', '1:5', '2:2', '2:7', '3:2', '3:7', 
       '4:2', '4:7', '5:3', '5:6', '6:1', '6:3', 
        '6:6', '6:8', '7:1', '7:2', '7:7', '7:8']
      pulsar (period 3):
      ['3:5', '3:6', '3:7', '3:11', '3:12', '3:13',
       '5:3', '5:8', '5:10', '5:15',
       '6:3', '6:8', '6:10', '6:15', 
       '7:3', '7:8', '7:10', '7:15',
       '8:5', '8:6', '8:7', '8:11', '8:12', '8:13',
       '10:5', '10:6', '10:7', '10:11', '10:12', '10:13',
       '11:3', '11:8', '11:10', '11:15',
       '12:3', '12:8', '12:10', '12:15',
       '13:3', '13:8', '13:10', '13:15',
       '15:5', '15:6', '15:7', '15:11', '15:12', '15:13']

Spaceships and gliders:

      glider:
      ['2:3', '3:4', '4:2', '4:3', '4:4']
      lwss (lightweight spaceship):
      ['2:2', '2:5', '3:6', '4:2', '4:6', '5:3', '5:4', '5:5', '5:6']
      gosper (gosper glider gun):
      ['6:2', '6:3', '7:2', '7:3', '4:14', '4:15', '5:13', '5:17', 
       '6:12', '6:18', '7:12', '7:16', '7:18', '7:19', '8:12', '8:18', 
       '9:13', '9:17', '10:14', '10:15', '2:26', '3:24', '3:26', '4:22', 
       '4:23', '5:22', '5:23', '6:22', '6:23', '7:24', '7:26', '8:26',
       '4:36', '4:37', '5:36', '5:37']

Methuselahs, mortal and otherwise:

      pentomino (R-pentimo):
      ['2:3', '2:4', '3:2', '3:3', '4:3']
      diehard (dies in 130 generations):
      ['2:8', '3:4', '3:4', '4:3', '4:7', '4:8', '4:9']
      acorn:
      ['2:3', '3:5', '4:2', '4:3', '4:6', '4:7', '4:8']
      blsse1 (block-laying switch engine 1):
      ['2:8', '3:6', '3:8', '3:9', '4:6', '4:8', '5:6', '6:4', '7:2', '7:4']
      blsse2 (block-laying switch engine 2):
      ['2:2', '2:3', '2:4', '2:6', '3:2', '4:5', '4:6', 
       '5:3', '5:4', '5:6', '6:2', '6:4', '6:6']
      linear1 (1-dimensional):
      ['2:2', '2:3', '2:4', '2:5', '2:6', '2:7', '2:8', '2:9',
       '2:11', '2:12', '2:13', '2:14', '2:15', '2:19', '2:20', '2:21',
       '2:28', '2:29', '2:30', '2:31', '2:32', '2:33', '2:34',
       '2:36', '2:37', '2:38', '2:39', '2:40']


===

Predefined rule sets:


1-D rules, from Wolfram

      rule90: 
          live [{0,dead,1}, {0,alive,1}, {1,dead,0},{1,alive,0},{1,alive,1}]
          die  [{0,dead,0}, {0,alive,0}, {1,dead,1}]
      rule30:
          live [{0,dead,1}, {0,alive,0}, {0,alive,1}, {1,dead,0}]
          die  [{1,alive,1}, {1,alive,0}, {1,dead,1}, {0,dead,0}]
      rule110:
          live [{0,dead,1}, {0,alive,0}, {0,alive,1}, {1,dead,1}, {1,alive,0}]
          die  [{0,dead,0}, {1,alive,1}]
      rule184:
          live [{0,alive,1}, {1,dead,0}, {1,dead,1}, {1,alive,1}]
          die  [{0,dead,0}, {0,dead,1}, {0,alive,0}, {1,alive,0}]


2-D rules, thanks to Mirek Wojtowicz, http://www.mirekw.com/ca:

      conway:
          {[3], [2,3]}
      life34:
          {[3,4], [3,4]}
      highlife:
          {[3,6], [2,3]}
      daynight:
          {[3,6,7,8], [3,4,6,7,8]}
      coral:
          {[3], [4,5,6,7,8]}
      coagulations:
          {[3,7,8], [2,3,5,6,7,8]}
      assimilation:
          {[3,4,5], [4,5,6,7]}
      amoeba:
          {[3,5,7], [1,3,5,8]}
      blocks2x2:
          {[3,6], [1,2,5]}
      flakes:
          {[3], [0,1,2,3,4,5,6,7,8]}
      gnarl:
          {[1], [1]}
      longlife:
          {[3,4,5], [5]}
      maze:
          {[3], [1,2,3,4,5]}
      mazectric:
          {[3], [1,2,3,4]}
      move:
          {[3,6,8], [2,4,5]}
      pseudolife:
          {[3,5,7], [2,3,8]}
      replicator:
          {[1,3,5,7], [1,3,5,7]}
      stains:
          {[3,6,7,8], [2,3,5,6,7,8]}
      walledcities:
          {[4,5,6,7,8], [2,3,4,5]}
