shadowsocks-erlang
===========
Totally asynchronous implementation of Shadowsocks in Erlang.

Original python version: https://github.com/clowwindy/shadowsocks

Usage
-----------
* Compile

    `./rebar clean & ./rebar compile`

  Elang OTP(R16B01) and rebar are needed. crypto module in OTP improves a lot in R16B01, but unfortunately, the port which implement this module does use EVP_* functions. Which means almost all the block cipher method is not compatible with other shadowsocks implementation. 
  default and rc4 method works fine.
  
* Client side

    `erl -smp auto -config ./local.config -pa ./ebin/ -boot start_sasl -s shadowsocks_app start -detached`
* Server side
   
   `erl -smp auto -config ./remote.config -pa ./ebin/ -boot start_sasl -s shadowsocks_app start -detached`

Configuration
-----------
* Config files

    `local.config, remote.config`
* Descriptions

```erlang
     {shadowsocks,
     [
       {type, local},
       {local_port,1080},
       {server,"localhost"},
       {server_port,8388},
       {password,"barfoo!"},
       {method,default}
     ]}
```

`{type, local}` is for client, `{type, remote}` is for server side.

`{error_logger, {file, "log/shadowsocks_client.log"}}`, logs will be written 
in `log/shadowsocks_client.log`. If you don't want it, use `{error_logger, silent}` instead

TODOs
-----------
* Check IPv6
* anthor crypto module which port(in C) uses EVP_* interfaces.

License
-----------
You are free to use, reuse and abuse all the code in this project, with following conditions are met:.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
