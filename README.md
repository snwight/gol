gol
===
Conway's Game of Life as an OTP compliant application 

Comprised of four erlang modules:
     gol_app.erl
     gol_supervisor.erl
     gol_server.erl
     gol_cell.erl

Run in erlang shell:

>>> gol_app:start(normal, [{ RowCount, ColumnCount }]).
    ...where RowCount is width of the 'world grid', ColumnCount is the height

>>> gol_server:seed([ CellKey1, CellKey2, CellKey3, ...CellKeyN ]).
    ...where CellKey is an atom in the following pattern:  'Row:Col' 
    ...see example seed sets below

>>> gol_server:display().
    ...prints current state of the world grid

>>> gol_server:clear().
    ...empties and displays the world grid

>>> gol_server:tick().
    ...executes a single global clock increment and displays refreshed world grid

>>> gol_server:run( N ).
    ...where N sets the finite number of ticks to execute, at 2 second intervals

===
Example seed states of interest (lifted from wikipedia examples):
These require a world grid of 12 by 12 size for best viewing.
 
Still Lifes:
      Block: 
      ['2:2', '2:3', '3:2', '3:3']
      Beehive: 
      ['2:3', '2:4', '3:2', '3:5', '4:4', '4:4']
      Loaf:
      ['2:3', '2:4', '3:2', '3:5', '4:3', '4:5', '5:4']
      Boat:
      ['2:2', '2:3', '3:2', '3:4', '4:3']

Oscillators:
      Blinker (period 2):
      ['3:2', '3:3', '3:4']
      Toad (period 2):
      ['3:3', '3:4', '3:5', '4:2', '4:3', '4:4']
      Beacon (period 2):
      ['2:2', '2:3', '3:2', '4:5', '5:4', '5:5']
      Fumarole (period 5):
      ['1:4', '1:5', '2:2', '2:7', '3:2', '3:7', '4:2', '4:7', '5:3', '5:6', '6:1', '6:3', '6:6', '6:8', '7:1', '7:2', '7:7', '7:8']
