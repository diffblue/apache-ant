package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JakartaRegexpRegexpDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link JakartaRegexpRegexp}
  */
  @Test
  public void testConstructor() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new JakartaRegexpRegexp()).getPattern());
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#getSubsOptions(int)}
   */
  @Test
  public void testGetSubsOptions() {
    // Arrange, Act and Assert
    assertEquals(1, (new JakartaRegexpRegexp()).getSubsOptions(1));
    assertEquals(0, (new JakartaRegexpRegexp()).getSubsOptions(Regexp.REPLACE_ALL));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jakartaRegexpRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute2() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("");

    // Act and Assert
    assertEquals("Argumentnput", jakartaRegexpRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute3() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("", jakartaRegexpRegexp.substitute("", "Argument", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute4() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jakartaRegexpRegexp.substitute("Input", "Argument", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute5() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jakartaRegexpRegexp.substitute("Input", "Argument", -1));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute6() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("");

    // Act and Assert
    assertEquals("ArgumentArgumentArgumentArgumentArgument", jakartaRegexpRegexp.substitute("Input", "Argument", -1));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute7() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("42");

    // Act and Assert
    assertEquals("Argument", jakartaRegexpRegexp.substitute("42", "Argument", 1));
  }

  /**
   * Method under test: {@link JakartaRegexpRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute8() throws BuildException {
    // Arrange
    JakartaRegexpRegexp jakartaRegexpRegexp = new JakartaRegexpRegexp();
    jakartaRegexpRegexp.setPattern("42");

    // Act and Assert
    assertEquals("Argument", jakartaRegexpRegexp.substitute("42", "Argument", RegexpMatcher.MATCH_CASE_INSENSITIVE));
  }
}

