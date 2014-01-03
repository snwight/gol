{application,gol,
             [{description,"OTP compliant Game of Life"},
              {vsn,"1"},
              {registered,[gol_app,gol_sup,gol_server,gol_cell]},
              {applications,[kernel,stdlib]},
              {mod,{gol_app,[]}},
              {env,[{dimensions,[20,20]}]},
              {modules,[gol_app,gol_cell,gol_server,gol_sup]}]}.
