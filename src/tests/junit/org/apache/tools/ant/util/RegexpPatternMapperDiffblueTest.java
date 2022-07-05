package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.util.regexp.Jdk14RegexpMatcher;
import org.junit.Test;

public class RegexpPatternMapperDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link RegexpPatternMapper}
  *   <li>{@link RegexpPatternMapper#setHandleDirSep(boolean)}
  * </ul>
  */
  @Test
  public void testConstructor() throws BuildException {
    // Arrange and Act
    RegexpPatternMapper actualRegexpPatternMapper = new RegexpPatternMapper();
    actualRegexpPatternMapper.setHandleDirSep(true);

    // Assert
    StringBuffer stringBuffer = actualRegexpPatternMapper.result;
    assertEquals(0, stringBuffer.length());
    assertEquals(JavaEnvUtils.VERSION_1_6, stringBuffer.capacity());
  }

  /**
   * Method under test: default or parameterless constructor of {@link RegexpPatternMapper}
   */
  @Test
  public void testConstructor2() throws BuildException {
    // Arrange and Act
    RegexpPatternMapper actualRegexpPatternMapper = new RegexpPatternMapper();

    // Assert
    assertTrue(actualRegexpPatternMapper.reg instanceof Jdk14RegexpMatcher);
    assertNull(actualRegexpPatternMapper.to);
  }

  /**
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new RegexpPatternMapper()).mapFileName("foo.txt"));
    assertNull((new RegexpPatternMapper()).mapFileName(null));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName2() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(true);

    // Act and Assert
    assertNull(regexpPatternMapper.mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName3() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setHandleDirSep(true);

    // Act and Assert
    assertNull(regexpPatternMapper.mapFileName("\\"));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName4() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setFrom("jane.doe@example.org");
    regexpPatternMapper.setTo("alice.liddell@example.org");

    // Act and Assert
    assertNull(regexpPatternMapper.mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName5() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setFrom("");
    regexpPatternMapper.setTo("alice.liddell@example.org");

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("alice.liddell@example.org", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link RegexpPatternMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName6() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setFrom("");
    regexpPatternMapper.setTo("\\");

    // Act
    String[] actualMapFileNameResult = regexpPatternMapper.mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("\\", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link RegexpPatternMapper#replaceReferences(String)}
   */
  @Test
  public void testReplaceReferences() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setTo("alice.liddell@example.org");
    regexpPatternMapper.setFrom("jane.doe@example.org");

    // Act and Assert
    assertEquals("alice.liddell@example.org", regexpPatternMapper.replaceReferences("Source"));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#replaceReferences(String)}
   */
  @Test
  public void testReplaceReferences2() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();
    regexpPatternMapper.setTo("\\");
    regexpPatternMapper.setFrom("jane.doe@example.org");

    // Act and Assert
    assertEquals("\\", regexpPatternMapper.replaceReferences("Source"));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();

    // Act
    regexpPatternMapper.setFrom("jane.doe@example.org");

    // Assert
    assertEquals("jane.doe@example.org", regexpPatternMapper.reg.getPattern());
  }

  /**
   * Method under test: {@link RegexpPatternMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom2() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RegexpPatternMapper()).setFrom(null));
  }

  /**
   * Method under test: {@link RegexpPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo() throws BuildException {
    // Arrange
    RegexpPatternMapper regexpPatternMapper = new RegexpPatternMapper();

    // Act
    regexpPatternMapper.setTo("alice.liddell@example.org");

    // Assert
    assertEquals(25, regexpPatternMapper.to.length);
  }

  /**
   * Method under test: {@link RegexpPatternMapper#setTo(String)}
   */
  @Test
  public void testSetTo2() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RegexpPatternMapper()).setTo(null));
  }
}

