if ($args.Count -eq 0)
{
	ghc -o main.exe Main.hs -odir object -hidir object
}
else
{
	switch ($args[0].ToLower())
	{
		'clean'
		{
			if (Test-Path main.exe)
			{
				Remove-Item 'main.exe'
			}
			
			Remove-Item 'object/*' -Recurse
		}
	}
}
