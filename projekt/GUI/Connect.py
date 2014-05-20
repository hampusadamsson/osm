import queue
import sys
import threading
from threading import Event
from time import sleep

class connectToServer(threading.Thread):

    def __init__(self,serverSocket,ipAdress):
        super(connectToServer,self).__init__()
        self.socket = serverSocket
        self.messageQueue = queue.Queue()
        self.ipAdress = ipAdress
        self.tries = 5

    def run(self):
        
        try:
            self.socket.connect((self.ipAdress,1337))
            self.messageQueue.put("Connected")
        except Exception as e:
            self.tries -=1
            self.messageQueue.put(self.tries)
            
        if(self.tries > 1):
            sleep(5)
            self.run()
        else:
            self.messageQueue.put("Failed")

    def returnQueue(self):
        if self.messageQueue.empty():
            return "empty"
        else:
            return self.messageQueue.get()
        
    def kill(self):
        sys.exit(0)        
        
