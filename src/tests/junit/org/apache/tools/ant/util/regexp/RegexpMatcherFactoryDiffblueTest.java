package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class RegexpMatcherFactoryDiffblueTest {
  /**
  * Method under test: {@link RegexpMatcherFactory#newRegexpMatcher(Project)}
  */
  @Test
  public void testNewRegexpMatcher() throws BuildException {
    // Arrange
    RegexpMatcherFactory regexpMatcherFactory = new RegexpMatcherFactory();

    Project project = new Project();
    project.addDataTypeDefinition("ant.regexp.regexpimpl", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(regexpMatcherFactory.newRegexpMatcher(project) instanceof Jdk14RegexpMatcher);
  }

  /**
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent() {
    // Arrange, Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(null));
  }

  /**
   * Method under test: {@link RegexpMatcherFactory#regexpMatcherPresent(Project)}
   */
  @Test
  public void testRegexpMatcherPresent2() {
    // Arrange
    Project project = new Project();
    project.addDataTypeDefinition("ant.regexp.regexpimpl", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(RegexpMatcherFactory.regexpMatcherPresent(project));
  }

  /**
   * Method under test: {@link RegexpMatcherFactory#testAvailability(String)}
   */
  @Test
  public void testTestAvailability() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RegexpMatcherFactory()).testAvailability("Class Name"));
  }
}

