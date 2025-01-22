package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.taskdefs.ExecuteOn.FileDirBoth;
import org.junit.Test;

public class TransformDiffblueTest {
  /**
   * Test new {@link Transform} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Transform}
   */
  @Test
  public void testNewTransform() {
    // Arrange and Act
    Transform actualTransform = new Transform();

    // Assert
    assertNull(actualTransform.destDir);
    assertNull(actualTransform.getDescription());
    assertNull(actualTransform.getTaskName());
    assertNull(actualTransform.getTaskType());
    assertNull(actualTransform.getOs());
    assertNull(actualTransform.getOsFamily());
    assertNull(actualTransform.getProject());
    assertNull(actualTransform.getOwningTarget());
    assertNull(actualTransform.srcFilePos);
    assertNull(actualTransform.targetFilePos);
    assertNull(actualTransform.mapperElement);
    assertNull(actualTransform.redirectorElement);
    assertNull(actualTransform.mapper);
    assertFalse(actualTransform.getResolveExecutable());
    assertFalse(actualTransform.failOnError);
    assertFalse(actualTransform.newEnvironment);
    assertTrue(actualTransform.filesets.isEmpty());
    assertTrue(actualTransform.srcIsFirst);
    assertEquals(FileDirBoth.FILE, actualTransform.type);
  }
}
