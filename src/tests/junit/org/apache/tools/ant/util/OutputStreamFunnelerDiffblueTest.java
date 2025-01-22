package org.apache.tools.ant.util;

import static org.junit.Assert.assertThrows;
import java.io.OutputStream;
import org.junit.Test;

public class OutputStreamFunnelerDiffblueTest {
  /**
   * Test {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream)}
   */
  @Test
  public void testNewOutputStreamFunneler_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new OutputStreamFunneler(null));
  }

  /**
   * Test {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream, long)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OutputStreamFunneler#OutputStreamFunneler(OutputStream, long)}
   */
  @Test
  public void testNewOutputStreamFunneler_whenNull_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new OutputStreamFunneler(null, 10L));

  }
}
