from collections import namedtuple
import math
from io import StringIO
import aoc

Packet = namedtuple('Packet', ('version', 'type', 'contents'))

def hex_to_bits(hex_value):
    return ''.join(f"{int(char, 16):04b}" for char in hex_value)

class BitStream:
    def __init__(self, hex_value):
        self.stream = StringIO(hex_to_bits(hex_value))

    def read(self, size):
        return int(self.stream.read(size), 2)

    def read_literal(self):
        output = ""
        more = True
        while more:
            more = self.read(1)
            output += self.stream.read(4)

        return int(output, 2)

    def tell(self):
        return self.stream.tell()

    def read_packet(self):
        packet_version = self.read(3)
        packet_type = self.read(3)

        if packet_type == 4:
            contents = self.read_literal()
        else:
            length_type_id = self.read(1)
            match length_type_id:
                case 0:
                    total_length = self.read(15)
                    start = self.tell()
                    packets = []
                    while self.tell() - start < total_length:
                        packets.append(self.read_packet())
                    contents = packets
                case 1:
                    packet_count = self.read(11)
                    contents = [self.read_packet() for _ in range(packet_count)]

        return Packet(version=packet_version, type=packet_type, contents=contents)

def parse_hex(hex_code):
    stream = BitStream(hex_code)
    return stream.read_packet()

def total_version(packet):
    version = packet.version
    if packet.type != 4:
        for value in packet.contents:
            version += total_version(value)
    return version

def eval(packet):
    match packet.type:
        case 0:
            return sum(eval(x) for x in packet.contents)
        case 1:
            return math.prod(eval(x) for x in packet.contents)
        case 2:
            return min(eval(x) for x in packet.contents)
        case 3:
            return max(eval(x) for x in packet.contents)
        case 4:
            return packet.contents
        case 5:
            if eval(packet.contents[0]) > eval(packet.contents[1]):
                return 1
            else:
                return 0
        case 6:
            if eval(packet.contents[0]) < eval(packet.contents[1]):
                return 1
            else:
                return 0
        case 7:
            if eval(packet.contents[0]) == eval(packet.contents[1]):
                return 1
            else:
                return 0


def main():
    assert parse_hex('D2FE28') == Packet(6, 4, 2021)
    assert parse_hex('38006F45291200') == Packet(1, 6, [Packet(6, 4, 10), Packet(2, 4, 20)])
    assert parse_hex('EE00D40C823060') == Packet(7, 3, [Packet(2, 4, 1), Packet(4, 4, 2), Packet(1, 4, 3)])
    assert total_version(parse_hex('8A004A801A8002F478')) == 16
    assert total_version(parse_hex('620080001611562C8802118E34')) == 12
    assert total_version(parse_hex('C0015000016115A2E0802F182340')) == 23
    assert total_version(parse_hex('A0016C880162017C3686B18A3D4780')) == 31

    line = aoc.lines(16)[0]

    parsed = parse_hex(line)
    print("Part 1: ", total_version(parsed))

    print("Part 2: ", eval(parsed))

if __name__ == '__main__':
    main()
