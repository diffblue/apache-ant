package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class RegexpMatcherFactoryDiffblueTest {
  /**
   * Test {@link RegexpMatcherFactory#newRegexpMatcher()}.
   * <p>
   * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher()}
   */
  @Test
  public void testNewRegexpMatcher() throws BuildException {
    // Arrange and Act
    RegexpMatcher actualNewRegexpMatcherResult = (new RegexpMatcherFactory()).newRegexpMatcher();

    // Assert
    assertTrue(actualNewRegexpMatcherResult instanceof Jdk14RegexpMatcher);
    assertNull(actualNewRegexpMatcherResult.getPattern());
  }

  /**
   * Test {@link RegexpMatcherFactory#newRegexpMatcher(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher(Project)}
   */
  @Test
  public void testNewRegexpMatcherWithProject_givenAntClassLoader() throws BuildException {
    // Arrange
    RegexpMatcherFactory regexpMatcherFactory = new RegexpMatcherFactory();

    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    // Act
    RegexpMatcher actualNewRegexpMatcherResult = regexpMatcherFactory.newRegexpMatcher(p);

    // Assert
    assertTrue(actualNewRegexpMatcherResult instanceof Jdk14RegexpMatcher);
    assertNull(actualNewRegexpMatcherResult.getPattern());
  }

  /**
   * Test {@link RegexpMatcherFactory#newRegexpMatcher(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher(Project)}
   */
  @Test
  public void testNewRegexpMatcherWithProject_givenJavaLangObject() throws BuildException {
    // Arrange
    RegexpMatcherFactory regexpMatcherFactory = new RegexpMatcherFactory();

    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    p.addBuildListener(new AntClassLoader());

    // Act
    RegexpMatcher actualNewRegexpMatcherResult = regexpMatcherFactory.newRegexpMatcher(p);

    // Assert
    assertTrue(actualNewRegexpMatcherResult instanceof Jdk14RegexpMatcher);
    assertNull(actualNewRegexpMatcherResult.getPattern());
  }

  /**
   * Test {@link RegexpMatcherFactory#newRegexpMatcher(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher(Project)}
   */
  @Test
  public void testNewRegexpMatcherWithProject_givenTarget() throws BuildException {
    // Arrange
    RegexpMatcherFactory regexpMatcherFactory = new RegexpMatcherFactory();

    Project p = new Project();
    p.addTarget("ant.PropertyHelper", new Target());
    p.addBuildListener(new AntClassLoader());

    // Act
    RegexpMatcher actualNewRegexpMatcherResult = regexpMatcherFactory.newRegexpMatcher(p);

    // Assert
    assertTrue(actualNewRegexpMatcherResult instanceof Jdk14RegexpMatcher);
    assertNull(actualNewRegexpMatcherResult.getPattern());
  }

  /**
   * Test {@link RegexpMatcherFactory#newRegexpMatcher(Project)} with {@code Project}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher(Project)}
   */
  @Test
  public void testNewRegexpMatcherWithProject_whenNull() throws BuildException {
    // Arrange and Act
    RegexpMatcher actualNewRegexpMatcherResult = (new RegexpMatcherFactory()).newRegexpMatcher(null);

    // Assert
    assertTrue(actualNewRegexpMatcherResult instanceof Jdk14RegexpMatcher);
    assertNull(actualNewRegexpMatcherResult.getPattern());
  }

  /**
   * Test {@link RegexpMatcherFactory#newRegexpMatcher(Project)} with {@code Project}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher(Project)}
   */
  @Test
  public void testNewRegexpMatcherWithProject_whenProject() throws BuildException {
    // Arrange
    RegexpMatcherFactory regexpMatcherFactory = new RegexpMatcherFactory();

    // Act
    RegexpMatcher actualNewRegexpMatcherResult = regexpMatcherFactory.newRegexpMatcher(new Project());

    // Assert
    assertTrue(actualNewRegexpMatcherResult instanceof Jdk14RegexpMatcher);
    assertNull(actualNewRegexpMatcherResult.getPattern());
  }

  /**
   * Test {@link RegexpMatcherFactory#testAvailability(String)}.
   * <p>
   * Method under test: {@link RegexpMatcherFactory#testAvailability(String)}
   */
  @Test
  public void testTestAvailability() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RegexpMatcherFactory()).testAvailability("Class Name"));
  }

  /**
   * Test {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent_givenAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(project));
  }

  /**
   * Test {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(project));
  }

  /**
   * Test {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent_givenTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(project));
  }

  /**
   * Test {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent_givenValue_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addReference("ant.PropertyHelper", "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse(RegexpMatcherFactory.regexpMatcherPresent(project));
  }

  /**
   * Test {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent_whenNull_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(null));
  }

  /**
   * Test {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent_whenProject_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(new Project()));
  }
}
