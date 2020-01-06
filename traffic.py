
import enum

STEP = 500  # Time step (seconds)
RWY_SEP = 90  # Runway area width

SHORTCUTS = """Shortcuts:
n: next time step
b: last time step
q: close window"""

class Point(object):
    """Meters coordinates, with attributes x, y: int"""

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return "({0.x}, {0.y})".format(self)

    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def __rmul__(self, k):
        return Point(k * self.x, k * self.y)

    def __abs__(self):
        return (self.x ** 2 + self.y ** 2) ** 0.5

    def sca(self, other):
        """sca(Point) return float
        returns the scalar product between self and other"""
        return self.x * other.x + self.y * other.y

    def det(self, other):
        """det(Point) return float
        returns the determinant between self and other"""
        return self.x * other.y - self.y * other.x

    def distance(self, other):
        return abs(self - other)

    def seg_dist(self, a, b):
        ab, ap, bp = b - a, self - a, self - b
        if ab.sca(ap) <= 0:
            return abs(ap)
        elif ab.sca(bp) >= 0:
            return abs(bp)
        else:
            return abs(ab.det(ap)) / abs(ap)

class Flight:
    """ Flight data, with the following attributes:
    - call_sign: str
    - start_t: int (beginning time step)
    - end_t: int (ending time step)
    - route: XY tuple (assigned route)"""

    def __init__(self, call_sign):
        self.call_sign = call_sign
        self.start_t = None
        self.end_t = None
        self.route = None
        self.headings = None

    def __repr__(self):
        return "<traffic.Flight {0}>".format(self.call_sign)

    def get_position(self, t):
        return self.route[t - self.start_t]

    def get_heading(self, t):
        return self.headings[t - self.start_t]

    def distance(self, other, t):
        return self.get_position(t).distance(other.get_position(t))

# Time string conversions

def hms(t):
    """hms(int) return str
    return a formatted string HH:MM:SS for the given time step"""
    s = t * STEP
    return "{:02d}:{:02d}:{:02d}".format(s // 3600, s // 60 % 60, s % 60)


def time_step(str_hms):
    """time_step(str) return int
    return the time step corresponding to a formatted string HH:MM:SS"""
    l = str_hms.replace(':', ' ').split() + [0, 0, 0]
    return (int(l[0]) * 3600 + int(l[1]) * 60 + int(l[2])) // STEP

def xys_to_points(str_xy_list):
    """ xys_to_points(str list) returns Point tuple: converts x,y str list to Point tuple"""

    def xy_to_point(str_xy):
        x, y, z = map(int, str_xy.split(','))
        return Point(x, y)

    return tuple(xy_to_point(str_xy) for str_xy in str_xy_list)

def xys_to_heading(str_xy_list):

    def xy_to_heading(str_xy):
        x, y, z = map(int, str_xy.split(','))
        return z

    return tuple(xy_to_heading(str_xy) for str_xy in str_xy_list)

# Load a traffic file

def from_file(filename):
    print("Loading traffic:", filename + '...')
    file = open(filename)
    flights = []
    for line in file:
        words = line.strip().split()
        try:
            flight = Flight(words[0])
            flight.route = xys_to_points(words[2:])
            flight.headings = xys_to_heading(words[2:])
            flight.start_t = int(words[1]) // STEP
            flight.end_t = flight.start_t + len(flight.route)
            flights.append(flight)
        except Exception as error:
            print(type(error), error, line)
    file.close()
    return flights


def select(flights, t):
    return [f for f in flights if f.start_t <= t < f.start_t + len(f.route)]
    

class Simulation:
    def __init__(self, flights, init_time = 0):
        self.all_flights = flights
        self.t = init_time
        self.current_flights = select(self.all_flights, self.t)

    def set_time(self, t):
        """set_time(int): set the current time to 't'"""
        self.t = t
        self.current_flights = select(self.all_flights, self.t)

    def increment_time(self, dt):
        """increment_time(int): increases the current time step by 'dt'
        (dt might be negative)"""
        self.set_time(self.t + dt)

