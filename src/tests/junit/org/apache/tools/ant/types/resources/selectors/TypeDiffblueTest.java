package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.selectors.Type.FileDir;
import org.junit.Test;

public class TypeDiffblueTest {
  /**
   * Test FileDir getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FileDir#FileDir()}
   *   <li>{@link FileDir#getValues()}
   * </ul>
   */
  @Test
  public void testFileDirGettersAndSetters() {
    // Arrange and Act
    FileDir actualFileDir = new FileDir();
    String[] actualValues = actualFileDir.getValues();

    // Assert
    assertNull(actualFileDir.getValue());
    assertEquals(-1, actualFileDir.getIndex());
    assertArrayEquals(new String[]{"file", "dir", "any"}, actualValues);
  }

  /**
   * Test FileDir {@link FileDir#FileDir(String)}.
   * <ul>
   *   <li>When {@code file}.</li>
   *   <li>Then return Value is {@code file}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileDir#FileDir(String)}
   */
  @Test
  public void testFileDirNewFileDir_whenFile_thenReturnValueIsFile() {
    // Arrange and Act
    FileDir actualFileDir = new FileDir("file");

    // Assert
    assertEquals("file", actualFileDir.getValue());
    assertEquals(0, actualFileDir.getIndex());
    assertArrayEquals(new String[]{"file", "dir", "any"}, actualFileDir.getValues());
  }

  /**
   * Test {@link Type#Type(FileDir)}.
   * <ul>
   *   <li>When {@link FileDir#FileDir(String)} with value is {@code any}.</li>
   *   <li>Then return Selected is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#Type(FileDir)}
   */
  @Test
  public void testNewType_whenFileDirWithValueIsAny_thenReturnSelectedIsNull() {
    // Arrange, Act and Assert
    assertTrue((new Type(new FileDir("any"))).isSelected(null));
  }

  /**
   * Test {@link Type#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Type#ANY} Type is {@code null}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Type#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenAnyTypeIsNull_whenResourceDirectoryIsTrue() {
    // Arrange
    Type type = Type.ANY;
    type.setType(null);

    Resource r = new Resource();
    r.setDirectory(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> type.isSelected(r));
  }
}
