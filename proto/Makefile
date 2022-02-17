.PHONY: all
all:
	erlc -buper +jer +noobj CoinProto.asn1
	mv CoinProto.hrl CoinProto.erl ../src/proto

.PHONY: clean
clean:
	rm -f CoinProto.asn1db CoinProto.beam
