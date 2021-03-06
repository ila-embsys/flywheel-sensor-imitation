cmake_minimum_required(VERSION 3.15)

set(APPLICATION_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/app")

# Set correct ZEPHYR_BASE var if we call
# CMake directly without west meta-tool
include(${CMAKE_CURRENT_SOURCE_DIR}/modules/zephyr-cmake-helpers/zephyr_loc.cmake)
extract_zephyr_base_loc("${CMAKE_SOURCE_DIR}/.west/config")
# Force correct Zephyr path
set(ENV{ZEPHYR_BASE} ${ZEPHYR_BASE_LOC})
message(STATUS "ZEPHYR_BASE was set as \"$ENV{ZEPHYR_BASE}\".")

# Zephyr manages optimisation flags by himself
unset(CMAKE_BUILD_TYPE CACHE)
message(STATUS "CMAKE_BUILD_TYPE was reset. Zephyr will care about it.")

# Zephyr initialization
include(${CMAKE_CURRENT_SOURCE_DIR}/modules/zephyr-cmake-helpers/zephyr.cmake)

# Define the top-level project because CMake
# wants it in the top-level CMakeLists.txt
project(app C CXX ASM ADA)

# Go to the actual app
add_subdirectory(app)
