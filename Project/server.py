
import asyncio
import argparse
import time
import json
import aiohttp
from collections import defaultdict
                                            
class Server:
    comm = defaultdict(set)
    assigned_ports = {'Hill': 11670, 'Jaquez':11671, 'Smith':11672, 'Campbell': 11673, 'Singleton':11674}
    friends = {'Hill': ['Jaquez', 'Smith'],
               'Jaquez': ['Hill', 'Singleton'],
               'Smith': ['Hill', 'Campbell', 'Singleton'],
               'Campbell': ['Smith', 'Singleton'],
               'Singleton': ['Jaquez', 'Smith', 'Campbell']}
    
    def __init__(self, name, ip='127.0.0.1', message_max_length=1e6):
        self.port = Server.assigned_ports[name]
        self.history = dict()
        self.log = open(name + '.log', 'a+')
        self.name = name
        self.ip = ip
        self.message_max_length = int(message_max_length)

    def __del__(self):
        if (hasattr(self, 'log')):
            self.log.close()
                    
    async def parse_message(self, message):
        message_list = [msg for msg in message.strip().split() if len(msg)]
        cmd = message_list[0]
        if (cmd == "IAMAT"):
            if len(message_list) != 4:
                return '? ' + message
            client_id, coordinates, timestamp = message_list[1:]
            # note that time difference can be negative due to clock skew
            # hence added some formatting for the time diff
            msg = f"AT {self.name} {time.time() - float(timestamp):+} {client_id} {coordinates} {timestamp}"
            if (self.name not in Server.comm[msg]):
                self.history[client_id] = msg
                Server.comm[msg].add(self.name)
                for friend in Server.friends[self.name]:
                    try:
                        reader, writer = await asyncio.open_connection(self.ip, Server.assigned_ports[friend])
                        writer.write(msg.encode())
                        await writer.drain()
                        self.log.write(f"{self.name} PROPAGATE: {msg} TO {friend}\n")
                        self.log.flush()
                        writer.close()
                    except ConnectionError:
                        self.log.write(f"{friend} IS DOWN\n")
                        self.log.flush()
            return msg
        elif (cmd == "WHATSAT"):
            if len(message_list) != 4:
                return '? ' + message
            client_id, radius, max_results = message_list[1:]
            if(float(radius) > 50 or int(max_results) > 20):
                return '? ' + message
            API_KEY = 'AIzaSyAA7gh9NBCnesxK1BgUUlW6tkksfxiktbw'
            # obtain longtitude and latitude, parse into url
            try:
                msg = self.history[client_id]
            except KeyError:
                return '? ' + message
            coordinates = msg.strip().split()[4]
            st = {"+","-"}
            ind = next((i for i, ch  in enumerate(coordinates[1:]) if ch in st),None)
            latitude = coordinates[:ind+1]
            longitude = coordinates[ind+1:]
            loc = "{0},{1}".format(latitude, longitude)
            url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(API_KEY, loc, radius)
            # use Google API
            async with aiohttp.ClientSession(
                    connector=aiohttp.TCPConnector(
                        ssl=False,
                    ),
            ) as session:
                async with session.get(url) as resp:
                    response = await resp.json()
                    response['results'] = response['results'][:int(max_results)]
                    google_api_feedback = json.dumps(response, indent=4)
                    return msg + "\n" + google_api_feedback + "\n\n"
        elif (cmd == "AT"):
            # add to history and propagate if not already known
            if len(message_list) != 6:
                return '? ' + message
            if (self.name not in Server.comm[message]):
                client_id = message_list[3]
                self.history[client_id] = message
                Server.comm[message].add(self.name)
                for friend in Server.friends[self.name]:
                    try:
                        reader, writer = await asyncio.open_connection(self.ip, Server.assigned_ports[friend])
                        writer.write(message.encode())
                        await writer.drain()
                        self.log.write(f"{self.name} PROPAGATE: {message} TO {friend}\n")
                        self.log.flush()
                        writer.close()
                    except ConnectionError:
                        self.log.write(f"{friend} IS DOWN\n")
                        self.log.flush()
            return ''
        else:
            return '? ' + message
        
        
    async def handle_message(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        print("{} RECEIVED {} FROM {}".format(self.name, message, addr))
        self.log.write("{} RECEIVED {} FROM {}\n".format(self.name, message, addr))
        self.log.flush()
        
        sendback_message = await self.parse_message(message)
        if (sendback_message != ''):
            print("{} SEND: {} TO CLIENT".format(self.name, sendback_message))
            self.log.write("{} SEND: {} TO CLIENT\n".format(self.name, sendback_message))
            self.log.flush()
            writer.write(sendback_message.encode())
            await writer.drain()

        print("close the client socket")
        writer.close()
    
    async def run_forever(self):
        server = await asyncio.start_server(self.handle_message, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()


def main():
    parser = argparse.ArgumentParser('argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    try:
        server = Server(args.server_name)
    except KeyError:
        print("server name not valid! Aborted.")
        return
    
    print("Hello, welcome to server {}".format(args.server_name))

    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
    main()
