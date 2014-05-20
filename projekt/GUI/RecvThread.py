import threading
import queue
import sys
from threading import Event

class StoppableThread(threading.Thread):

    def __init__(self,serverSocket):
        super(StoppableThread, self).__init__()
        self.stopSignal = threading.Event()
        self.serverSocket = serverSocket.makefile()
        self.messageQueue = queue.Queue()

    def stop(self):
        self.stopSignal.set()

    def stopped(self):
        return self.stopSignal.isSet()

    def run(self):
         while True:
            if (self.stopped()):
                print("Nu sticker tråden med, toodlezzz")
                sys.exit(0)
            else:
                data = self.serverSocket.readline()
                print(data)
                self.messageQueue.put(data)
        

    def returnQueue(self):
        if self.messageQueue.empty():
            return "empty"
        else:
            return self.messageQueue.get()
<<<<<<< HEAD
=======

>>>>>>> persnusk
