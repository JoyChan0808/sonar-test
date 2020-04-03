package com.aam.producer.playlist.app;

import com.aam.producer.playlist.sal.client.IJobClient;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.time.FastDateFormat;
import org.junit.Test;

//@RunWith(SpringRunner.class)
//@SpringBootTest(classes = PlaylistApplication.class)
public class JobserviceTest {

    //@Autowired
    IJobClient iJobClient;

    //@Test
    public void test01() {
        Map<String, String> paramMap = new HashMap<>();
        paramMap.put("playlistTitle", "aaaax");
        paramMap.put("cron", "0 0/5 * * * ?");
        paramMap.put("orgUuid", "1");
        paramMap.put("pplUuid", "2");
        paramMap.put("versionUuid", "3");
        iJobClient.add(paramMap);
    }


    //@Test
    public void test02() {
        iJobClient.remove("38");
    }


    @Test
    public void test03() {
        Calendar cal = Calendar.getInstance();
        int startDay = Calendar.SUNDAY;
        cal.setFirstDayOfWeek(startDay);
        int dayOfWeek = cal.get(Calendar.DAY_OF_WEEK);
        if (dayOfWeek > startDay) {
            cal.set(Calendar.DATE, cal.get(Calendar.DATE) - (dayOfWeek - startDay));
        } else {
            cal.set(Calendar.DATE, cal.get(Calendar.DATE) - (7 - (startDay - dayOfWeek)));
        }
        FastDateFormat fastDateFormat = FastDateFormat.getInstance("yyyy/MM/dd");
        System.out.println(fastDateFormat.format(cal.getTime()));
    }
}
