package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.util.regexp.Jdk14RegexpRegexp;
import org.apache.tools.ant.util.regexp.Regexp;
import org.junit.Test;

public class RegularExpressionDiffblueTest {
  /**
   * Test {@link RegularExpression#getPattern(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getPattern(Project)}
   */
  @Test
  public void testGetPattern_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(regularExpression.getPattern(p));
  }

  /**
   * Test {@link RegularExpression#getPattern(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getPattern(Project)}
   */
  @Test
  public void testGetPattern_givenJavaLangObject() {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(regularExpression.getPattern(p));
  }

  /**
   * Test {@link RegularExpression#getPattern(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getPattern(Project)}
   */
  @Test
  public void testGetPattern_givenTarget_whenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    Project p = new Project();
    p.addTarget("ant.PropertyHelper", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(regularExpression.getPattern(p));
  }

  /**
   * Test {@link RegularExpression#getPattern(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getPattern(Project)}
   */
  @Test
  public void testGetPattern_whenNull() {
    // Arrange, Act and Assert
    assertNull((new RegularExpression()).getPattern(null));
  }

  /**
   * Test {@link RegularExpression#getPattern(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getPattern(Project)}
   */
  @Test
  public void testGetPattern_whenProject() {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    // Act and Assert
    assertNull(regularExpression.getPattern(new Project()));
  }

  /**
   * Test {@link RegularExpression#getRegexp(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getRegexp(Project)}
   */
  @Test
  public void testGetRegexp_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act
    Regexp actualRegexp = regularExpression.getRegexp(p);

    // Assert
    assertTrue(actualRegexp instanceof Jdk14RegexpRegexp);
    assertNull(actualRegexp.getPattern());
  }

  /**
   * Test {@link RegularExpression#getRegexp(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getRegexp(Project)}
   */
  @Test
  public void testGetRegexp_givenJavaLangObject() throws BuildException {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act
    Regexp actualRegexp = regularExpression.getRegexp(p);

    // Assert
    assertTrue(actualRegexp instanceof Jdk14RegexpRegexp);
    assertNull(actualRegexp.getPattern());
  }

  /**
   * Test {@link RegularExpression#getRegexp(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>When {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getRegexp(Project)}
   */
  @Test
  public void testGetRegexp_givenTarget_whenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    Project p = new Project();
    p.addTarget("ant.PropertyHelper", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act
    Regexp actualRegexp = regularExpression.getRegexp(p);

    // Assert
    assertTrue(actualRegexp instanceof Jdk14RegexpRegexp);
    assertNull(actualRegexp.getPattern());
  }

  /**
   * Test {@link RegularExpression#getRegexp(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getRegexp(Project)}
   */
  @Test
  public void testGetRegexp_whenNull() throws BuildException {
    // Arrange and Act
    Regexp actualRegexp = (new RegularExpression()).getRegexp(null);

    // Assert
    assertTrue(actualRegexp instanceof Jdk14RegexpRegexp);
    assertNull(actualRegexp.getPattern());
  }

  /**
   * Test {@link RegularExpression#getRegexp(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RegularExpression#getRegexp(Project)}
   */
  @Test
  public void testGetRegexp_whenProject() throws BuildException {
    // Arrange
    RegularExpression regularExpression = new RegularExpression();

    // Act
    Regexp actualRegexp = regularExpression.getRegexp(new Project());

    // Assert
    assertTrue(actualRegexp instanceof Jdk14RegexpRegexp);
    assertNull(actualRegexp.getPattern());
  }

  /**
   * Test new {@link RegularExpression} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link RegularExpression}
   */
  @Test
  public void testNewRegularExpression() {
    // Arrange and Act
    RegularExpression actualRegularExpression = new RegularExpression();

    // Assert
    assertEquals("RegularExpression", actualRegularExpression.getDataTypeName());
    Location location = actualRegularExpression.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRegularExpression.getDescription());
    assertNull(actualRegularExpression.getProject());
    assertNull(actualRegularExpression.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualRegularExpression.isReference());
    assertTrue(actualRegularExpression.isChecked());
  }
}
