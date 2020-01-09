import math
import traffic

from PyQt5 import QtWidgets, QtGui, QtCore
from PyQt5.QtGui import QPen, QBrush, QColor, QPixmap
from PyQt5.QtCore import Qt, QSize, QRectF
from PyQt5.QtWidgets import QGraphicsEllipseItem, QGraphicsPixmapItem


# constants
WIDTH = 800  # Initial window width (pixels)
HEIGHT = 450  # Initial window height (pixels)
PLOT_Z_VALUE = 1
ANIMATION_DELAY = 50  # milliseconds


# creating the brush
BRUSH = QBrush(QColor("blue"))


class PanZoomView(QtWidgets.QGraphicsView):
    """An interactive view that supports Pan and Zoom functions"""

    def __init__(self, scene):
        super().__init__(scene)
        # enable anti-aliasing
        self.setRenderHint(QtGui.QPainter.Antialiasing)
        # enable drag and drop of the view
        self.setDragMode(self.ScrollHandDrag)

        self.setSceneRect(QRectF(-150000,-150000,300000,300000))

    def wheelEvent(self, event):
        """Overrides method in QGraphicsView in order to zoom it when mouse scroll occurs"""
        factor = math.pow(1.001, event.angleDelta().y())
        self.zoom_view(factor)

    @QtCore.pyqtSlot(int)
    def zoom_view(self, factor):
        """Updates the zoom factor of the view"""
        self.setTransformationAnchor(self.AnchorUnderMouse)
        super().scale(factor, factor)


class RadarView(QtWidgets.QWidget):
    """An interactive view of an airport and its flights,
    with the following attributes:
    - scene: QtWidgets.QGraphicsScene (the graphic scene)
    - view: QtWidgets.QGraphicsView (the view of the scene)
    - moving_aircraft: radarmotion.AircraftItemsMotionManager  """

    def __init__(self, simu):
        super().__init__()
        self.simulation = simu
        self.time_increment = 1

        # Settings
        self.setWindowTitle('test')#'Airport Sim at ' + self.simulation.airport.name)
        self.resize(WIDTH, HEIGHT)

        # create components
        root_layout = QtWidgets.QVBoxLayout(self)
        self.scene = QtWidgets.QGraphicsScene()
        self.view = PanZoomView(self.scene)
        self.time_entry = QtWidgets.QLineEdit()
        toolbar = self.create_toolbar()

        # invert y axis for the view
        self.view.scale(1, -1)

        # maintain a scene graph so as to _update_ plots
        # instead of clearing and recreating them at each update
        self.moving_aircraft = AircraftItemsMotionManager(self)

        # add components to the root_layout
        root_layout.addWidget(self.view)
        root_layout.addLayout(toolbar)

        # create and setup the timer
        self.timer = QtCore.QTimer(self)
        self.timer.timeout.connect(self.advance)

        self.fit_scene_in_view()

        # show the window
        self.show()

    def create_toolbar(self):
        # create layout for time controls and entry
        toolbar = QtWidgets.QHBoxLayout()

        def add_button(text, slot):
            """adds a button to the hbox and connects the slot"""
            button = QtWidgets.QPushButton(text)
            button.clicked.connect(slot)
            toolbar.addWidget(button)

        # lambda function allows to pass extra arguments to slots
        # added space around '-' character to avoid different look and feel
        add_button('-', lambda: self.view.zoom_view(1/1.1))
        add_button('+', lambda: self.view.zoom_view(1.1))
        toolbar.addStretch()
        add_button('<<', lambda: self.set_time_increment(-5))
        add_button(' <', lambda: self.set_time_increment(-1))
        add_button('|>', self.playpause)
        add_button(' >', lambda: self.set_time_increment(1))
        add_button('>>', lambda: self.set_time_increment(5))
        toolbar.addWidget(self.time_entry)
        self.time_entry.setInputMask("00:00:00")
        self.time_entry.editingFinished.connect(self.change_time)
        self.time_entry.setText(traffic.hms(self.simulation.t))
        toolbar.addStretch()

        # shortcuts and key bindings
        def add_shortcut(text, slot):
            """creates an application-wide key binding"""
            shortcut = QtWidgets.QShortcut(QtGui.QKeySequence(text), self)
            shortcut.activated.connect(slot)

        add_shortcut('-', lambda: self.view.zoom_view(1/1.1))
        add_shortcut('+', lambda: self.view.zoom_view(1.1))
        add_shortcut(' ', self.playpause)
        add_shortcut('q', QtCore.QCoreApplication.instance().quit)
        return toolbar

    def fit_scene_in_view(self):
        self.view.fitInView(self.view.sceneRect(), QtCore.Qt.KeepAspectRatio)

    def update_traffic(self):
        self.moving_aircraft.update_aircraft_items()
        self.time_entry.setText(traffic.hms(self.simulation.t))

    @QtCore.pyqtSlot()
    def change_time(self):
        """slot triggered when a new time is input in the text field"""
        self.simulation.set_time(traffic.time_step(self.time_entry.text()))
        self.time_entry.clearFocus()
        self.update_traffic()

    @QtCore.pyqtSlot()
    def advance(self):
        """this slot computes the new time at each time out"""
        self.simulation.increment_time(self.time_increment)
        self.update_traffic()

    @QtCore.pyqtSlot(int)
    def set_time_increment(self, dt):
        """this slot updates the speed of the replay"""
        self.time_increment = dt

    @QtCore.pyqtSlot()
    def playpause(self):
        """this slot toggles the replay using the timer as model"""
        if self.timer.isActive():
            self.timer.stop()
        else:
            self.timer.start(ANIMATION_DELAY)
            
class AircraftItemsMotionManager:
    """Collection of aircraft items and their motion management"""

    def __init__(self, radar):
        # reference to the radar view
        self.radarView = radar
        # list of the current flights
        self.current_flights = []
        # dictionary of the corresponding aircraft items in the scene
        self.aircraft_items_dict = {}

        # populate flight list and aircraft items dictionary then create and update the corresponding aircraft items
        self.update_aircraft_items()

    def update_aircraft_items(self):
        """ updates Plots views """
        new_flights = self.radarView.simulation.current_flights
        # add new aircraft items for flights who joined
        for f in set(new_flights) - set(self.current_flights):
            item = AircraftItem(self.radarView.simulation, f)  # create an item
            self.radarView.scene.addItem(item)  # add it to scene
            self.aircraft_items_dict[f] = item  # add it to item dict
        # remove aircraft items for flights who left
        for f in set(self.current_flights) - set(new_flights):
            item = self.aircraft_items_dict.pop(f)  # get item from flight in the dictionary (and remove it)
            self.radarView.scene.removeItem(item)   # remove it also from scene
        # refresh current flights list
        self.current_flights = new_flights
       # update positions of the current aircraft items
        for aircraft in self.aircraft_items_dict.values():
            aircraft.update_position(aircraft.flight)# in conf)


        
class AircraftItem(QGraphicsPixmapItem):
    """The view of an aircraft in the GraphicsScene"""
    def __init__(self, simu, f):
        """AircraftItem constructor, creates the ellipse and adds to the scene"""
        super().__init__(None)
        self.setZValue(PLOT_Z_VALUE)

        # instance variables
        self.flight = f
        self.simulation = simu
        #build the Pixmap
        self.setPixmap(QPixmap("DATA/airplane.png"))
        self.setOffset(-self.boundingRect().width()/2, -self.boundingRect().height()/2)
        self.setScale(10)
        # add tooltip
        tooltip = f.call_sign
        self.setToolTip(tooltip)

    def mousePressEvent(self, event):
        event.accept()

    def update_position(self, is_conflict):
        """moves the plot in the scene"""
        position = self.flight.get_position(self.simulation.t)
        self.setPos(position.x, position.y)
        heading = self.flight.get_heading(self.simulation.t)
        self.setRotation(heading)
