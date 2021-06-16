#include <array>
#include <kernel.h>
#include <logging/log.h>

#include <drivers/adc.h>

#define ADC_NUM_CHANNELS DT_PROP_LEN(DT_NODELABEL(adc1), io_channels)

#define ADC_NODE DT_LABEL(DT_NODELABEL(adc1))

LOG_MODULE_REGISTER(main);

extern "C" {
extern void update_rpm(int current_rpm);
}

const struct device* dev_adc = device_get_binding(ADC_NODE);
static constexpr uint8_t CHANNEL_COUNT = 4;
std::array<uint16_t, CHANNEL_COUNT> measurements;
std::array<adc_sequence, CHANNEL_COUNT> sequences = {
  adc_sequence{
    .channels = 1 << 0,
    .buffer = &measurements[0],
    .buffer_size = sizeof(measurements[0]),
    .resolution = 12,
  },

  adc_sequence{
    .channels = 1 << 1,
    .buffer = &measurements[1],
    .buffer_size = sizeof(measurements[1]),
    .resolution = 12,
  },

  adc_sequence{
    .channels = 1 << 2,
    .buffer = &measurements[2],
    .buffer_size = sizeof(measurements[2]),
    .resolution = 12,
  },

  adc_sequence{
    .channels = 1 << 3,
    .buffer = &measurements[3],
    .buffer_size = sizeof(measurements[3]),
    .resolution = 12,
  }};

void main()
{
    LOG_INF("Start Ada execution!");

    while (true) {
        for (auto& i : sequences) {
            adc_read(dev_adc, &i);
        }
        update_rpm(*(int*)(sequences[0].buffer));
        k_sleep(K_MSEC(100));
    }
}
