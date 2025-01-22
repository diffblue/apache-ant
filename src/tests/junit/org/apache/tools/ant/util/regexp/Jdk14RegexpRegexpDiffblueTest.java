package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class Jdk14RegexpRegexpDiffblueTest {
  /**
   * Test {@link Jdk14RegexpRegexp#getSubsOptions(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpRegexp#getSubsOptions(int)}
   */
  @Test
  public void testGetSubsOptions_whenOne_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, (new Jdk14RegexpRegexp()).getSubsOptions(1));
  }

  /**
   * Test {@link Jdk14RegexpRegexp#getSubsOptions(int)}.
   * <ul>
   *   <li>When {@link Regexp#REPLACE_ALL}.</li>
   *   <li>Then return {@link Regexp#REPLACE_ALL}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpRegexp#getSubsOptions(int)}
   */
  @Test
  public void testGetSubsOptions_whenReplace_all_thenReturnReplace_all() {
    // Arrange, Act and Assert
    assertEquals(Regexp.REPLACE_ALL, (new Jdk14RegexpRegexp()).getSubsOptions(Regexp.REPLACE_ALL));
  }

  /**
   * Test {@link Jdk14RegexpRegexp#substitute(String, String, int)}.
   * <ul>
   *   <li>Given {@link Jdk14RegexpRegexp} (default constructor) Pattern is {@code Pattern}.</li>
   *   <li>When one.</li>
   *   <li>Then return {@code Input}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute_givenJdk14RegexpRegexpPatternIsPattern_whenOne_thenReturnInput() throws BuildException {
    // Arrange
    Jdk14RegexpRegexp jdk14RegexpRegexp = new Jdk14RegexpRegexp();
    jdk14RegexpRegexp.setPattern("Pattern");

    // Act and Assert
    assertEquals("Input", jdk14RegexpRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Test {@link Jdk14RegexpRegexp#substitute(String, String, int)}.
   * <ul>
   *   <li>Then return {@code ArgumentInput}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute_thenReturnArgumentInput() throws BuildException {
    // Arrange
    Jdk14RegexpRegexp jdk14RegexpRegexp = new Jdk14RegexpRegexp();
    jdk14RegexpRegexp.setPattern("");

    // Act and Assert
    assertEquals("ArgumentInput", jdk14RegexpRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Test {@link Jdk14RegexpRegexp#substitute(String, String, int)}.
   * <ul>
   *   <li>When ninety-two.</li>
   *   <li>Then return {@code Input}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Jdk14RegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute_whenNinetyTwo_thenReturnInput() throws BuildException {
    // Arrange
    Jdk14RegexpRegexp jdk14RegexpRegexp = new Jdk14RegexpRegexp();
    jdk14RegexpRegexp.setPattern("Pattern");

    // Act and Assert
    assertEquals("Input", jdk14RegexpRegexp.substitute("Input", "Argument", 92));
  }

  /**
   * Test new {@link Jdk14RegexpRegexp} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Jdk14RegexpRegexp}
   */
  @Test
  public void testNewJdk14RegexpRegexp() {
    // Arrange, Act and Assert
    assertNull((new Jdk14RegexpRegexp()).getPattern());
  }
}
