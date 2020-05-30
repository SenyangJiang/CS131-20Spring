
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

    def write_to_log(self, msg):
        self.log.write(msg)
        self.log.flush()

    # write message to client and close the socket
    async def write_to_client(self, writer, msg):
        writer.write(msg.encode())
        await writer.drain()
        self.write_to_log(f"{self.name} SENT: {msg} TO CLIENT\n")
        writer.close()
        
    # flood message to other servers
    # add to history if not already known
    async def flood(self, msg, client_id):
        if (self.name in Server.comm[msg]):
            self.write_to_log(f"REPEATED MESSAGE: {msg}\n")
            return
        
        self.history[client_id] = msg
        Server.comm[msg].add(self.name)
        for friend in Server.friends[self.name]:
            try:
                reader, writer = await asyncio.open_connection(self.ip, Server.assigned_ports[friend])
                self.write_to_log(f"{self.name} CONNECTED TO {friend}\n")
                writer.write(msg.encode())
                await writer.drain()
                self.write_to_log(f"{self.name} PROPAGATE: {msg} TO {friend}\n")
                writer.close()
                self.write_to_log(f"{self.name} CLOSED CONNECTION TO {friend}\n")
            except ConnectionError:
                self.write_to_log(f"{self.name}: ERROR CONNECTING TO {friend}\n")

    # fetch from google map
    # return value is a JSON object
    async def fetch(self, url):
        # use Google API
        async with aiohttp.ClientSession(
                connector=aiohttp.TCPConnector(
                    ssl=False,
                ),
        ) as session:
            async with session.get(url) as resp:
                return await resp.json()
            
    async def handle_i_am_at(self, writer, message_list, message):
        if len(message_list) != 4:
            await self.write_to_client(writer, '? ' + message)
            return
        client_id, coordinates, timestamp = message_list[1:]
        # note that time difference can be negative due to clock skew
        # hence added some formatting for the time diff
        msg = f"AT {self.name} {time.time() - float(timestamp):+} {client_id} {coordinates} {timestamp}"
        await self.write_to_client(writer, msg)
        await self.flood(msg, client_id)
    
    async def handle_whats_at(self, writer, message_list, message):
        if len(message_list) != 4:
            await self.write_to_client(writer, '? ' + message)
            return
        client_id, radius, max_results = message_list[1:]
        if(float(radius) > 50 or int(max_results) > 20):
            await self.write_to_client(writer, '? ' + message)
            return
        API_KEY = 'AIzaSyAA7gh9NBCnesxK1BgUUlW6tkksfxiktbw'
        # obtain longtitude and latitude, parse into url
        try:
            msg = self.history[client_id]
        except KeyError:
            self.write_to_log(f"{self.name}: {client_id} NOT FOUND IN HISTORY\n")
            await self.write_to_client(writer, '? ' + message)
        coordinates = msg.strip().split()[4]
        st = {"+","-"}
        ind = next((i for i, ch  in enumerate(coordinates[1:]) if ch in st),None)
        latitude = coordinates[:ind+1]
        longitude = coordinates[ind+1:]
        loc = "{0},{1}".format(latitude, longitude)
        url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(API_KEY, loc, radius)
        response = await self.fetch(url)
        response['results'] = response['results'][:int(max_results)]
        google_api_feedback = json.dumps(response, indent=4)
        await self.write_to_client(writer, msg + "\n" + google_api_feedback + "\n\n")
        
    async def handle_at(self, writer, message_list, message):
        # add to history and propagate if not already known
        if len(message_list) != 6:
            await self.write_to_client(writer, '? ' + message)
            return
        client_id = message_list[3]
        await self.flood(message, client_id)
        writer.close()    
        
    async def handle_message(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        print(f"{self.name} RECEIVED {message} FROM {addr}")
        self.write_to_log(f"{self.name} RECEIVED: {message}\n")
        message_list = [msg for msg in message.strip().split() if len(msg)]
        cmd = message_list[0]
        if (cmd == "IAMAT"):
            await self.handle_i_am_at(writer, message_list, message)
        elif (cmd == "WHATSAT"):
            await self.handle_whats_at(writer, message_list, message)
        elif (cmd == "AT"):
            await self.handle_at(writer, message_list, message)
        else:
            await self.write_to_client(writer, '? ' + message)
        
    
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
