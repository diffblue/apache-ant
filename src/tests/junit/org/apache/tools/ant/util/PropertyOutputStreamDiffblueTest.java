package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class PropertyOutputStreamDiffblueTest {
  /**
  * Method under test: {@link PropertyOutputStream#PropertyOutputStream(Project, String)}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals(0, (new PropertyOutputStream(new Project(), "foo")).size());
    assertEquals(0, (new PropertyOutputStream(new Project(), "foo", true)).size());
  }
}

