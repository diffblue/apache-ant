package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class SunRmicDiffblueTest {
  /**
   * Test {@link SunRmic#preprocessCompilerArgs(String[])}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Compiler Args}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunRmic#preprocessCompilerArgs(String[])}
   */
  @Test
  public void testPreprocessCompilerArgs_thenReturnArrayOfStringWithCompilerArgs() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Compiler Args"},
        (new SunRmic()).preprocessCompilerArgs(new String[]{"Compiler Args"}));
  }

  /**
   * Test new {@link SunRmic} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SunRmic}
   */
  @Test
  public void testNewSunRmic() {
    // Arrange and Act
    SunRmic actualSunRmic = new SunRmic();

    // Assert
    assertNull(actualSunRmic.getRmic());
    assertNull(actualSunRmic.getMapper());
  }
}
