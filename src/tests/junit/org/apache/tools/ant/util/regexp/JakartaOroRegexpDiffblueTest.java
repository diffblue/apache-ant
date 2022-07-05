package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.oro.text.regex.Perl5Matcher;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class JakartaOroRegexpDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link JakartaOroRegexp}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    Perl5Matcher perl5Matcher = (new JakartaOroRegexp()).matcher;
    assertFalse(perl5Matcher.isMultiline());
    assertNull(perl5Matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#getSubsOptions(int)}
   */
  @Test
  public void testGetSubsOptions() {
    // Arrange, Act and Assert
    assertEquals(1, (new JakartaOroRegexp()).getSubsOptions(1));
    assertEquals(-1, (new JakartaOroRegexp()).getSubsOptions(Regexp.REPLACE_ALL));
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jakartaOroRegexp.substitute("Input", "Argument", 1));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute2() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("");

    // Act and Assert
    assertEquals("ArgumentInput", jakartaOroRegexp.substitute("Input", "Argument", 1));
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute3() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("", jakartaOroRegexp.substitute("", "Argument", 1));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute4() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jakartaOroRegexp.substitute("Input", "Argument", 92));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute5() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("foo");

    // Act and Assert
    assertEquals("Input", jakartaOroRegexp.substitute("Input", "Argument", -1));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute6() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("");

    // Act and Assert
    assertEquals("Argument", jakartaOroRegexp.substitute("", "Argument", 1));
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute7() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("");

    // Act and Assert
    assertEquals("Input", jakartaOroRegexp.substitute("Input", "", 1));
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute8() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("");

    // Act and Assert
    assertEquals("ArgumentIArgumentnArgumentpArgumentuArgumenttArgument",
        jakartaOroRegexp.substitute("Input", "Argument", 92));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute9() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("");

    // Act and Assert
    assertEquals("ArgumentIArgumentnArgumentpArgumentuArgumenttArgument",
        jakartaOroRegexp.substitute("Input", "Argument", -1));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute10() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("iogmsx-");

    // Act and Assert
    assertEquals("Input", jakartaOroRegexp.substitute("Input", "Argument", -1));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }

  /**
   * Method under test: {@link JakartaOroRegexp#substitute(String, String, int)}
   */
  @Test
  public void testSubstitute11() throws BuildException {
    // Arrange
    JakartaOroRegexp jakartaOroRegexp = new JakartaOroRegexp();
    jakartaOroRegexp.setPattern("iogmsx-");

    // Act and Assert
    assertEquals("Argument", jakartaOroRegexp.substitute("iogmsx-", "Argument", -1));
    assertNull(jakartaOroRegexp.matcher.getMatch());
  }
}

