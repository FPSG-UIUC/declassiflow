#==============================================================================================
# ADD THE TARGET
#==============================================================================================
add_library(declassiflow SHARED
    util.cpp
    passes.cpp
    cleanup.cpp
    loop.cpp
    cloning.cpp
    flagging.cpp
    relaxation.cpp
    declassiflow.cpp
)

target_include_directories(declassiflow PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../include")

# Allow undefined symbols in shared objects on Darwin (this is the default
# behaviour on Linux)
target_link_libraries(declassiflow "$<$<PLATFORM_ID:Darwin>:-undefined dynamic_lookup>")
target_link_libraries(declassiflow ${YAML_CPP_LIBRARIES})