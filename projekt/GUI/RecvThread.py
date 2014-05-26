import threading
import queue
import sys
from threading import Event

class StoppableThread(threading.Thread):

    def __init__(self,serverSocket):
        super(StoppableThread, self).__init__()
        self.serverSocket = serverSocket.makefile()
        self.messageQueue = queue.Queue()

    def run(self):
        while True:
            data = self.serverSocket.readline()

            #Om vi bara far bullshit fran socketen avslutas traden och GUI:t
            #informeras att detta har hant med "Disconnect" meddelandet

            if not data:
                #Avbryt loopen
                break
            self.messageQueue.put(data)
        print("Nu dog servern, ahahahahahahaha\n")
        self.messageQueue.put("Disconnected")
        sys.exit(0)
        

    def returnQueue(self):
        if self.messageQueue.empty():
            return "empty"
        else:
            return self.messageQueue.get()
