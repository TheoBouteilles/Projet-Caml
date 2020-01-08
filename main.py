"""Main module for the Python Airport code"""
import sys

from PyQt5 import QtWidgets
import traffic
import radarview


if __name__ == "__main__":
    flights = traffic.from_file(sys.argv[1])

    # create the simulation
    sim = traffic.Simulation(flights)

    # Initialize Qt
    app = QtWidgets.QApplication([])

    # create the radar view and the time navigation interface
    main_window = radarview.RadarView(sim)

    print(traffic.SHORTCUTS)

    # enter the main loop
    app.exec_()
