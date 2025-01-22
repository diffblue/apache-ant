package org.apache.tools.ant.types.optional.depend;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.optional.depend.ClassfileSet.ClassRoot;
import org.junit.Test;

public class ClassfileSetDiffblueTest {
  /**
   * Test ClassRoot getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ClassRoot}
   *   <li>{@link ClassRoot#setClassname(String)}
   *   <li>{@link ClassRoot#getClassname()}
   * </ul>
   */
  @Test
  public void testClassRootGettersAndSetters() {
    // Arrange and Act
    ClassRoot actualClassRoot = new ClassRoot();
    actualClassRoot.setClassname("Name");

    // Assert
    assertEquals("Name", actualClassRoot.getClassname());
  }

  /**
   * Test {@link ClassfileSet#ClassfileSet()}.
   * <p>
   * Method under test: {@link ClassfileSet#ClassfileSet()}
   */
  @Test
  public void testNewClassfileSet() {
    // Arrange and Act
    ClassfileSet actualClassfileSet = new ClassfileSet();

    // Assert
    assertNull(actualClassfileSet.getDir());
    Location location = actualClassfileSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualClassfileSet.getDescription());
    assertNull(actualClassfileSet.getProject());
    assertNull(actualClassfileSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualClassfileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualClassfileSet.isReference());
    assertTrue(actualClassfileSet.getDefaultexcludes());
    assertTrue(actualClassfileSet.getErrorOnMissingDir());
    assertTrue(actualClassfileSet.isFilesystemOnly());
  }

  /**
   * Test {@link ClassfileSet#ClassfileSet(ClassfileSet)}.
   * <ul>
   *   <li>When {@link ClassfileSet#ClassfileSet()}.</li>
   *   <li>Then return Dir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassfileSet#ClassfileSet(ClassfileSet)}
   */
  @Test
  public void testNewClassfileSet_whenClassfileSet_thenReturnDirIsNull() {
    // Arrange and Act
    ClassfileSet actualClassfileSet = new ClassfileSet(new ClassfileSet());

    // Assert
    assertNull(actualClassfileSet.getDir());
    Location location = actualClassfileSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualClassfileSet.getDescription());
    assertNull(actualClassfileSet.getProject());
    assertNull(actualClassfileSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualClassfileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualClassfileSet.isReference());
    assertTrue(actualClassfileSet.getDefaultexcludes());
    assertTrue(actualClassfileSet.getErrorOnMissingDir());
    assertTrue(actualClassfileSet.isFilesystemOnly());
  }

  /**
   * Test {@link ClassfileSet#clone()}.
   * <p>
   * Method under test: {@link ClassfileSet#clone()}
   */
  @Test
  public void testClone() {
    // Arrange and Act
    Object actualCloneResult = (new ClassfileSet()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof ClassfileSet);
    assertNull(((ClassfileSet) actualCloneResult).getDir());
    Location location = ((ClassfileSet) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((ClassfileSet) actualCloneResult).getDescription());
    assertNull(((ClassfileSet) actualCloneResult).getProject());
    assertNull(((ClassfileSet) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, ((ClassfileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((ClassfileSet) actualCloneResult).isReference());
    assertTrue(((ClassfileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((ClassfileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((ClassfileSet) actualCloneResult).isFilesystemOnly());
  }
}
