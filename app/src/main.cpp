#include <array>
#include <kernel.h>
#include <logging/log.h>

#include <drivers/adc.h>
#include <drivers/gpio.h>

#define ADC_NUM_CHANNELS DT_PROP_LEN(DT_NODELABEL(adc1), io_channels)

#define ADC_NODE DT_LABEL(DT_NODELABEL(adc1))


#if DT_NODE_HAS_STATUS(DT_NODELABEL(sensor1), okay)
// #define SENSOR_NODE DT_LABEL(DT_NODELABEL(sensor1))
#define SENSOR_NODE	DT_GPIO_LABEL(DT_NODELABEL(sensor1), gpios)
#define SENSOR_PIN	DT_GPIO_PIN(DT_NODELABEL(sensor1), gpios)
#define SENSOR_FLAGS	DT_GPIO_FLAGS(DT_NODELABEL(sensor1), gpios)
#else
/* A build error here means your board isn't set up to blink an LED. */
#error "Unsupported board: sensor devicetree alias is not defined"
#define SENSOR	""
#define SENSOR_PIN	0
#define SENSOR_FLAGS	0
#endif

#define ADC_DEVICE_NAME         ADC_NODE
#define ADC_RESOLUTION		12
#define ADC_GAIN		ADC_GAIN_1
#define ADC_REFERENCE		ADC_REF_INTERNAL
#define ADC_ACQUISITION_TIME	ADC_ACQ_TIME_DEFAULT
#define ADC_1ST_CHANNEL_ID	0

#define BUFFER_SIZE  6
static int16_t m_sample_buffer[BUFFER_SIZE];


LOG_MODULE_REGISTER(main);

extern "C" {
extern void update_rpm(const int current_rpm, float* next_tooth_time);
extern void time_to_tooth();
}

const struct device* dev_gpio = device_get_binding(SENSOR_NODE);
static constexpr uint8_t CHANNEL_COUNT = 4;

const struct adc_sequence sequence = {
  .channels    = BIT(ADC_1ST_CHANNEL_ID),
  .buffer      = m_sample_buffer,
  .buffer_size = sizeof(m_sample_buffer),
  .resolution  = ADC_RESOLUTION,
};

static const struct adc_channel_cfg m_1st_channel_cfg = {
	.gain             = ADC_GAIN,
	.reference        = ADC_REFERENCE,
	.acquisition_time = ADC_ACQUISITION_TIME,
	.channel_id       = ADC_1ST_CHANNEL_ID,
#if defined(CONFIG_ADC_CONFIGURABLE_INPUTS)
	.input_positive   = ADC_1ST_CHANNEL_INPUT,
#endif
};

static const struct device *init_adc(void)
{
	int ret;
  const struct device *adc_dev = device_get_binding(ADC_DEVICE_NAME);

	ret = adc_channel_setup(adc_dev, &m_1st_channel_cfg);

	(void)memset(m_sample_buffer, 0, sizeof(m_sample_buffer));

	return adc_dev;
}

void main()
{
    int ret;
    const struct device* dev_adc = init_adc();
    gpio_pin_configure(dev_gpio, SENSOR_PIN, GPIO_OUTPUT_ACTIVE | SENSOR_FLAGS);

    LOG_INF("Start Ada execution!");

    while (true) {
        ret = adc_read(dev_adc, &sequence);
        float next_tooth_time;
        const int rpm = m_sample_buffer[0];
        update_rpm(rpm, &next_tooth_time);
        unsigned next_tooth_time_usec = static_cast<unsigned>(next_tooth_time * 1000000);
        k_sleep(K_USEC(next_tooth_time_usec));
        time_to_tooth();
    }
}

extern "C"
void set_pin(int state)
{
  gpio_pin_set(dev_gpio, SENSOR_PIN, state);
}
