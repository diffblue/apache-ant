package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class Jdk14RegexpRegexpDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link Jdk14RegexpRegexp}
  */
  @Test
  public void testConstructor() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new Jdk14RegexpRegexp()).getPattern());
  }

  /**
   * Method under test: {@link Jdk14RegexpRegexp#getSubsOptions(int)}
   */
  @Test
  public void testGetSubsOptions() {
    // Arrange, Act and Assert
    assertEquals(1, (new Jdk14RegexpRegexp()).getSubsOptions(1));
    assertEquals(Regexp.REPLACE_ALL, (new Jdk14RegexpRegexp()).getSubsOptions(Regexp.REPLACE_ALL));
  }

  /**
   * Method under test: {@link Jdk14RegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute() throws BuildException {
    // Arrange
    Jdk14RegexpRegexp jdk14RegexpRegexp = new Jdk14RegexpRegexp();
    jdk14RegexpRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jdk14RegexpRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Method under test: {@link Jdk14RegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute2() throws BuildException {
    // Arrange
    Jdk14RegexpRegexp jdk14RegexpRegexp = new Jdk14RegexpRegexp();
    jdk14RegexpRegexp.setPattern("");

    // Act and Assert
    assertEquals("ArgumentInput", jdk14RegexpRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Method under test: {@link Jdk14RegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute3() throws BuildException {
    // Arrange
    Jdk14RegexpRegexp jdk14RegexpRegexp = new Jdk14RegexpRegexp();
    jdk14RegexpRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jdk14RegexpRegexp.substitute("Input", "Argument", 92));
  }
}

