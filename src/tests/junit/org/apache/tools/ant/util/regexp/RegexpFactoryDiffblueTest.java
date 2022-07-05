package org.apache.tools.ant.util.regexp;

import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class RegexpFactoryDiffblueTest {
  /**
  * Method under test: {@link RegexpFactory#newRegexp(Project)}
  */
  @Test
  public void testNewRegexp() throws BuildException {
    // Arrange
    RegexpFactory regexpFactory = new RegexpFactory();

    Project project = new Project();
    project.addDataTypeDefinition("ant.regexp.regexpimpl", Object.class);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertTrue(regexpFactory.newRegexp(project) instanceof Jdk14RegexpRegexp);
  }
}

