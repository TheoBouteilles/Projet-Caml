"""Main module for the Python Airport code"""

from PyQt5 import QtWidgets
import traffic
import radarview

PLN_FILE = ("DATA/flights.txt","DATA/lfpg_flights.txt", "DATA/lfpo_flights.txt")

if __name__ == "__main__":
    choice = 0
    flights = traffic.from_file(PLN_FILE[choice])

    # create the simulation
    sim = traffic.Simulation(flights)

    # Initialize Qt
    app = QtWidgets.QApplication([])

    # create the radar view and the time navigation interface
    main_window = radarview.RadarView(sim)

    print(traffic.SHORTCUTS)

    # enter the main loop
    app.exec_()
