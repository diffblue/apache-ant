package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class RegexpFactoryDiffblueTest {
  /**
   * Test {@link RegexpFactory#newRegexp()}.
   * <p>
   * Method under test: {@link RegexpFactory#newRegexp()}
   */
  @Test
  public void testNewRegexp() throws BuildException {
    // Arrange and Act
    Regexp actualNewRegexpResult = (new RegexpFactory()).newRegexp();

    // Assert
    assertTrue(actualNewRegexpResult instanceof Jdk14RegexpRegexp);
    assertNull(actualNewRegexpResult.getPattern());
  }

  /**
   * Test {@link RegexpFactory#newRegexp(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpFactory#newRegexp(Project)}
   */
  @Test
  public void testNewRegexpWithProject_givenAntClassLoader() throws BuildException {
    // Arrange
    RegexpFactory regexpFactory = new RegexpFactory();

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act
    Regexp actualNewRegexpResult = regexpFactory.newRegexp(p);

    // Assert
    assertTrue(actualNewRegexpResult instanceof Jdk14RegexpRegexp);
    assertNull(actualNewRegexpResult.getPattern());
  }

  /**
   * Test {@link RegexpFactory#newRegexp(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpFactory#newRegexp(Project)}
   */
  @Test
  public void testNewRegexpWithProject_givenJavaLangObject() throws BuildException {
    // Arrange
    RegexpFactory regexpFactory = new RegexpFactory();

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act
    Regexp actualNewRegexpResult = regexpFactory.newRegexp(p);

    // Assert
    assertTrue(actualNewRegexpResult instanceof Jdk14RegexpRegexp);
    assertNull(actualNewRegexpResult.getPattern());
  }

  /**
   * Test {@link RegexpFactory#newRegexp(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpFactory#newRegexp(Project)}
   */
  @Test
  public void testNewRegexpWithProject_givenTarget() throws BuildException {
    // Arrange
    RegexpFactory regexpFactory = new RegexpFactory();

    Project p = new Project();
    p.addTarget("ant.PropertyHelper", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act
    Regexp actualNewRegexpResult = regexpFactory.newRegexp(p);

    // Assert
    assertTrue(actualNewRegexpResult instanceof Jdk14RegexpRegexp);
    assertNull(actualNewRegexpResult.getPattern());
  }

  /**
   * Test {@link RegexpFactory#newRegexp(Project)} with {@code Project}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpFactory#newRegexp(Project)}
   */
  @Test
  public void testNewRegexpWithProject_whenNull() throws BuildException {
    // Arrange and Act
    Regexp actualNewRegexpResult = (new RegexpFactory()).newRegexp(null);

    // Assert
    assertTrue(actualNewRegexpResult instanceof Jdk14RegexpRegexp);
    assertNull(actualNewRegexpResult.getPattern());
  }

  /**
   * Test {@link RegexpFactory#newRegexp(Project)} with {@code Project}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpFactory#newRegexp(Project)}
   */
  @Test
  public void testNewRegexpWithProject_whenProject() throws BuildException {
    // Arrange
    RegexpFactory regexpFactory = new RegexpFactory();

    // Act
    Regexp actualNewRegexpResult = regexpFactory.newRegexp(new Project());

    // Assert
    assertTrue(actualNewRegexpResult instanceof Jdk14RegexpRegexp);
    assertNull(actualNewRegexpResult.getPattern());
  }
}
