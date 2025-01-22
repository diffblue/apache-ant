package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class IdentityMapperDiffblueTest {
  /**
   * Test {@link IdentityMapper#mapFileName(String)}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_whenFooTxt_thenReturnArrayOfStringWithFooTxt() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, (new IdentityMapper()).mapFileName("foo.txt"));
  }

  /**
   * Test {@link IdentityMapper#mapFileName(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IdentityMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new IdentityMapper()).mapFileName(null));
  }
}
