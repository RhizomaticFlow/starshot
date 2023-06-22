# Starshot
Particle-based Cartesian Physics

Fundamental Classes:
- Cartesian (3-dimensional vector)
- Particle (point-based entities)
- Spring ('forces' modelled as classical springs) TODO

Kinds of Systems:
- P0 (Particles Only)
- P1 (Particles and Springs)

TODO:
- Modelling gravity and/or charge? Are these particle specific? Should they be in an inherited class, or just attributes? 
- Polar-coordinates?
- Radiation?
- Optics? Wave mechanics? TISE?
- Multi-threading?
- Improvements to collisions E.G., manually moving them out the others box
- Improvements to graphical rendering E.G., not just squares

## Usage & Installation
(ql:quickload :starshot)
(starshot/graphics:particles)

## Testing
(asdf:test-system 'starshot)
