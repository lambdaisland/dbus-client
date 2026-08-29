# 0.4.30 (2026-08-29 / ec11cd9)

## Added

- `lambdaisland.dbus.bus` - wrapper for the org.freedesktop.DBus interface (list-names, request-name, add-match, get-id, etc.)
- `lambdaisland.dbus.std` - wrapper for org.freedesktop.DBus.Peer, org.freedesktop.DBus.Properties, org.freedesktop.DBus.ObjectManager interfaces
- `lambdaisland.dbus.message` - utility functions over dbus message maps and responses
- `lambdaisland.dbus.systemd` - wrapper for org.freedesktop.systemd1.Manager (list-units, start-unit, etc.)
- High-level `(client/call member dest path & args)` entry point
- Automatically get method signature from introspection
- Type derivation (e.g. for variants) handles non-vector sequences as arrays (vectors become structs)

## Fixed

- Only return a promise and block if a reply is expected
- Fixed array/struct alignment issues
- Fixes to message buffering, handle multiple messages in a single read, or partial message in a single read
- Dispatch handler on `singleThreadExecutor`, prevent blocked handler from blocking the message loop, while retaining ordering guarantees

## Changed

- Type derivation treats longs as int32 if they are within range, or int64 otherwise (before was always int64)

# 0.2.15 (2025-11-20 / 5438609)

## Added

- Start adding CLI interface

# 0.1.11 (2025-11-15 / 5b63506)

- First release
